(library (sph lang sc expressions)
  (export
    add-begin-if-multiple
    contains-set?
    not-function-pointer-symbol?
    not-preprocessor-keyword?
    preprocessor-keyword?
    sc-apply
    sc-case
    sc-compile-type
    sc-compile-types
    sc-cond
    sc-cond*
    sc-declare
    sc-define
    sc-define-array
    sc-define-function
    sc-define-type
    sc-do-while
    sc-enum
    sc-for
    sc-function
    sc-function-pointer
    sc-function-pointer?
    sc-identifier
    sc-identifier-list
    sc-if
    sc-if*
    sc-include-sc
    sc-include-sc-once
    sc-join-expressions
    sc-let*
    sc-macro-function
    sc-no-semicolon
    sc-numeric-boolean
    sc-path->full-path
    sc-pre-cond
    sc-pre-define
    sc-pre-define-if-not-defined
    sc-pre-define-multiple
    sc-pre-if
    sc-pre-include
    sc-pre-include-define
    sc-pre-include-variable
    sc-pre-let*
    sc-set
    sc-set-multiple
    sc-state-load-paths
    sc-state-new
    sc-state-no-semicolon
    sc-struct-literal
    sc-struct-or-union
    sc-struct-or-union-body
    sc-value
    sc-while)
  (import
    (ice-9 match)
    (ice-9 regex)
    (rnrs exceptions)
    (rnrs io simple)
    (sph)
    (sph alist)
    (sph filesystem)
    (sph hashtable)
    (sph lang c expressions)
    (sph lang scheme)
    (sph list)
    (sph string)
    (only (guile)
      compose
      make-regexp
      negate
      string-contains
      string-index
      string-join
      string-null?
      string-prefix?
      string-skip
      string-split
      string-suffix?
      string-trim-right)
    (only (sph tree) tree-contains?)
    (only (sph vector) vector-accessor))

  (define sc-state-load-paths (vector-accessor 1))
  (define sc-state-no-semicolon (vector-accessor 2))
  (define (sc-state-new load-paths) (vector (q sc-state) load-paths (ht-create-symbol)))

  (define sph-lang-sc-expressions-description
    "bindings for creating c strings from sc specific expressions.
     when to add parentheses
       returned expressions should always be unambiguosly one expression
         example: (+ 1 2 3) -> (1+2+3)
       when received expressions can have undesired meaning, add parentheses
         (struct-set **a b 1) -> (**a).b=1")

  (define (sc-no-semicolon-register state name)
    "symbol -> unspecified
     add no semicolon after name when used"
    (ht-set! (sc-state-no-semicolon state) name #t))

  (define (sc-no-semicolon-unregister state name) (ht-delete! (sc-state-no-semicolon state) name))
  (define (sc-no-semicolon-registered? state name) (ht-ref (sc-state-no-semicolon state) name))

  (define (sc-no-semicolon expressions compile)
    "(sc ...) -> c
     add no semicolon after any expression. for macros that expand to function definitions whose body must not
     end with a semicolon according to iso c11"
    (string-join (map compile expressions) "\n" (q suffix)))

  (define (sc-no-semicolon-track state macro-name a)
    "iso c11 does not allow semicolons at the end of function bodies like {};
     because sc can not know from macro application syntax if it expands to a function definition,
     it needs to track all macro definitions to decide if a semicolon needs to be added.
     this does not work when using macro definitions from preprocessor included c code -
     in this case only sc-no-semicolon expressions, sc-insert or a compiler option (enabled by default in gcc) can solve this"
    (match a (((quote begin) initial ... last) (sc-no-semicolon-track state macro-name last))
      (((quote define) (name parameters ...) body ...) (sc-no-semicolon-register state macro-name))
      (else a)))

  (define-syntax-rule (add-begin-if-multiple a) (if (= 1 (length a)) (first a) (pair (q begin) a)))
  (define (contains-set? a) "list -> boolean" (and (list? a) (tree-contains? a (q set))))
  (define (preprocessor-keyword? a) (and (symbol? a) (string-prefix? "pre-" (symbol->string a))))
  (define not-preprocessor-keyword? (negate preprocessor-keyword?))
  (define (not-function-pointer-symbol? a) (not (and (symbol? a) (eq? (q function-pointer) a))))

  (define (add-begin a)
    (if (and (list? a) (not (null? a)) (equal? (q begin) (first a))) a (list (q begin) a)))

  (define (alist->regexp-match-replacements a)
    "automatically converts strings at the prefix position to regular expressions"
    (map (l (e) (pair (if (string? (first e)) (make-regexp (first e)) (first e)) (tail e))) a))

  (define identifier-replacements
    (alist->regexp-match-replacements
      ; (regexp search-string . replacement)
      ; replaced in order
      ; mostly following the c identifier conversion rules used in guile.
      ; https://www.gnu.org/software/guile/manual/html_node/API-Overview.html#API-Overview
      (alist "->" "_to_"
        ".-" (pair "-" "_")
        "-." (pair "-" "_")
        ".!" (pair "!" "_x")
        "\\?" "_p"
        "./." (pair "/" "_or_")
        ".<" (pair "<" "_less")
        ".>" (pair ">" "_gr") ".<=" (pair "<=" "_leq") ".>=" (pair ">=" "_geq"))))

  (define sc-identifier-prefixes (list #\& #\*))
  (define sc-identifier-infixes (list #\. #\:))

  (define (sc-identifier-struct-pointer-get a)
    (let (a (string-split a #\:)) (if (= 1 (length a)) (first a) (apply c-struct-pointer-get a))))

  (define (translate-identifier a)
    (let
      ( (a (regexp-match-replace a identifier-replacements))
        (contains-infix (any (l (char) (string-index a char)) sc-identifier-infixes))
        (after-prefix-index (string-skip a (l (a) (containsq? sc-identifier-prefixes a)))))
      (if (and after-prefix-index (not (zero? after-prefix-index)))
        (if contains-infix
          (string-append (substring a 0 after-prefix-index)
            (parenthesise (sc-identifier-struct-pointer-get (substring a after-prefix-index))))
          a)
        (if contains-infix (sc-identifier-struct-pointer-get a) a))))

  (define (sc-identifier a)
    (if (symbol? a) (translate-identifier (symbol->string a))
      (if (list? a) (string-join (map sc-identifier a) " ") (any->string a))))

  (define (sc-let* a compile)
    (c-compound
      (match a
        ( ( ( (names values ...) ...) body ...)
          (compile
            (pair (q begin)
              (append
                (map (l (n v) (pairs (if (= 1 (length v)) (q set) (q define)) n v)) names values)
                body)))))))

  (define (sc-pre-define-if-not-defined a compile)
    (pair (q begin)
      (map-slice 2
        (l (name value)
          (let (identifier (match name (((? not-preprocessor-keyword? name) _ ...) name) (_ name)))
            (qq
              (pre-if-not-defined (unquote identifier)
                (unquote
                  (if value (qq (pre-define (unquote name) (unquote value)))
                    (qq (pre-define (unquote name)))))))))
        (if (= 1 (length a)) (list (first a) #f) a))))

  (define (sc-do-while a compile)
    (match a
      ( (test body ...)
        (string-append "do" (c-compound (compile (pair (q begin) body)))
          "while" (parenthesise (compile test))))))

  (define (sc-pre-let* a compile)
    (match a
      ( ( (names+values ...) body ...)
        (string-replace-string
          (string-append
            (string-join (map-slice 2 (l (n v) (compile (list (q pre-define) n v))) names+values)
              "\n" (q suffix))
            (compile (pair (q begin) body)) "\n"
            (string-join
              (map-slice 2 (l (n v) (compile (list (q pre-undefine) (if (pair? n) (first n) n))))
                names+values)
              "\n"))
          "\n\n" "\n"))
      (_ (raise (q syntax-error-for-pre-let*)))))

  (define (sc-for a compile)
    (let
      (comma-join
        (l (a)
          (match a (((quote begin) a ...) (string-join (map compile a) ","))
            (((? symbol?) _ ...) (compile a)) (_ (string-join (map compile a) ",")))))
      (match a
        ( ( (init test update) body ...)
          (c-for (comma-join init) (compile test)
            (comma-join update) (compile (pair (q begin) body)))))))

  (define (sc-numeric-boolean prefix a compile)
    (let (operator (if (eq? (q =) prefix) "==" (symbol->string prefix)))
      (string-join
        (map-segments 2 (l (a b) (parenthesise (string-append a operator b)))
          (map (l (a) (if (contains-set? a) (parenthesise (compile a)) (compile a))) a))
        "&&")))

  (define (sc-if a compile)
    (match a
      ( (test consequent alternate)
        (c-if-statement (compile test) (compile (list (q begin) consequent))
          (compile (list (q begin) alternate))))
      ((test consequent) (c-if-statement (compile test) (compile (list (q begin) consequent))))))

  (define (sc-semicolon-list-to-comma-list a)
    "this would not work with string or character literals that contain semicolons.
     if these literals can occur anywhere other than definitions this needs to be changed"
    (parenthesise (string-replace-string (string-trim-right a #\;) ";" ",")))

  (define (sc-if* a compile)
    (apply c-if
      (map
        (l (b)
          (match b
            ( ( (quote begin) body ...)
              (parenthesise
                (string-join
                  (map
                    (l (b)
                      (if (contains-set? b) (sc-semicolon-list-to-comma-list (compile b))
                        (compile b)))
                    body)
                  ",")))
            (_ (if (contains-set? b) (sc-semicolon-list-to-comma-list (compile b)) (compile b)))))
        a)))

  (define (sc-apply state name a compile)
    (string-append
      (c-apply (compile name) (string-join (map (compose parenthesise-ambiguous compile) a) ","))
      (if (sc-no-semicolon-registered? state name) "\n" "")))

  (define* (sc-join-expressions a #:optional (expression-separator ""))
    "main procedure for the concatenation of toplevel expressions"
    (define (fold-f b prev)
      (pair
        (cond
          ( (string-prefix? "#" b)
            ; preprocessor directives need to be on a separate line
            (if (and (not (null? prev)) (string-suffix? "\n" (first prev))) (string-append b "\n")
              (string-append "\n" b "\n")))
          ( (string-prefix? "/*" b)
            (if (and (not (null? prev)) (string-suffix? "\n" (first prev))) b
              (string-append "\n" b)))
          ((or (string-suffix? "\n" b) (string-suffix? ";" b)) b)
          ((string-suffix? ":" b) (string-append b "\n"))
          (else (string-append b ";")))
        prev))
    (apply string-append (reverse (fold fold-f null (remove string-null? a)))))

  (define* (sc-define-type compile name value) "any any -> string"
    (let (name (compile name))
      (if (sc-function-pointer? value)
        (string-append "typedef " (apply sc-function-pointer compile name (tail value)))
        (c-typedef name (compile value)))))

  (define (sc-compile-type a compile)
    (if (list? a)
      (if
        (or (null? a)
          (let (a-first (first a))
            (not
              (and (symbol? a-first)
                (or (preprocessor-keyword? a-first) (eq? (q function-pointer) a-first))))))
        (string-join (map sc-identifier a) " ") (compile a))
      (sc-identifier a)))

  (define (sc-compile-types a compile)
    (parenthesise (string-join (map (l (e) (sc-compile-type e compile)) a) ",")))

  (define (sc-function-pointer? a)
    (and (list? a) (not (null? a)) (eq? (q function-pointer) (first a))))

  (define (sc-function-pointer compile inner type-output . type-input)
    "procedure string ? ? ... -> string"
    (if (sc-function-pointer? type-output)
      (apply sc-function-pointer compile
        (string-append (parenthesise (string-append "*" inner))
          (sc-compile-types type-input compile))
        (tail type-output))
      (string-append (sc-compile-type type-output compile) (parenthesise (string-append "*" inner))
        (sc-compile-types type-input compile))))

  (define (get-body-and-docstring& body compile macro-function? c)
    "list procedure:{string:docstring string:body -> any} -> any"
    (if (null? body) (c #f #f)
      (apply
        (l (docstring body)
          (c docstring
            (if macro-function? (string-trim-right (sc-join-expressions (map compile body)) #\;)
              (if (null? body) #f (sc-join-expressions (map compile body))))))
        (if macro-function?
          (match (first body)
            ( ( (quote begin) (? string? docstring) body ...)
              (list (docstring->comment docstring) body))
            (_ (list #f body)))
          (if (string? (first body)) (list (docstring->comment (first body)) (tail body))
            (list #f body))))))

  (define (docstring->comment a) (string-append "\n/** " a " */\n"))

  (define (sc-function compile name return-type body parameter-names parameter-types)
    (let
      ( (parameters (sc-function-parameters compile parameter-names parameter-types name))
        (name (compile name)))
      (get-body-and-docstring& body compile
        #f
        (l (docstring body-string)
          (let (body-string (if body-string (c-curly-brackets body-string) ""))
            (string-append (or docstring "")
              (if (sc-function-pointer? return-type)
                (string-append
                  (apply sc-function-pointer compile
                    (string-append name parameters) (tail return-type))
                  body-string)
                (string-append (sc-compile-type return-type compile) " "
                  name parameters body-string))
              ; only declarations get a semicolon at the end
              (if (string-null? body-string) "" "\n")))))))

  (define (sc-macro-function name parameter body compile)
    (get-body-and-docstring& body compile
      #t
      (l (docstring body-string)
        (string-append (or docstring "")
          (string-replace-string
            (string-trim-right
              (cp-define (sc-identifier name) body-string (sc-identifier-list parameter)) #\newline)
            "\n" "\\\n")
          (if docstring "\n" "")))))

  (define (sc-function-parameter compile name type)
    (if (sc-function-pointer? type) (apply sc-function-pointer compile (compile name) (tail type))
      (string-append (sc-compile-type type compile) " " (compile name))))

  (define (sc-function-parameters compile names types function-name)
    (parenthesise
      (if (list? names)
        (string-join (map (l a (apply sc-function-parameter compile a)) names types) ",")
        (if (or (symbol? names) (string? names))
          (string-append (compile types) " " (compile names))
          (raise (q cannot-convert-to-c-parameter))))))

  (define (sc-identifier-list a) (parenthesise (string-join (map sc-identifier a) ",")))

  (define (sc-value a)
    (cond
      ((symbol? a) (translate-identifier (symbol->string a)))
      ((boolean? a) (if a "1" "0"))
      (else (c-value a))))

  (define (sc-pre-if type a compile)
    (match a
      ( (test consequent alternate)
        (cp-if type (compile test) (compile (add-begin consequent)) (compile (add-begin alternate))))
      ((test consequent) (cp-if type (compile test) (compile (add-begin consequent)) #f))))

  (define (sc-case is-case* a compile compile->sc)
    "boolean list procedure -> list:sc
     expands so cond expressions"
    (compile->sc
      (match a
        ( (predicate subject clauses ..1)
          (pair (if is-case* (q cond*) (q cond))
            (map
              (l (a)
                (match a (((quote else) _ ...) a)
                  ( ( (objects ...) body ...)
                    (pair (pair (q or) (map (l (b) (list predicate b subject)) objects)) body))
                  ((object body ...) (pair (list predicate object subject) body))))
              clauses))))))

  (define (sc-pre-define-multiple a compile) "-> list"
    (match a
      ( (name-1 value-1 name-2 value-2 rest ...)
        (pair (q begin) (map-slice 2 (l a (pair (q pre-define) a)) (map compile a))))
      (_ #f)))

  (define (sc-pre-define state a compile) "-> string"
    (match a ((name) (cp-define (sc-identifier name)))
      ( (name value)
        (match name
          ( (name parameter ...) (sc-no-semicolon-track state name value)
            (sc-macro-function name parameter (list value) compile))
          (_ (sc-no-semicolon-track state name value)
            (cp-define (sc-identifier name)
              (string-replace-string (string-trim-right (compile value) #\newline) "\n" "\\\n")))))
      (_ #f)))

  (define (sc-pre-include paths)
    "(string ...) -> string
     uses c load path <> notation when a given path does not start with a slash or with a directory reference"
    (string-join
      (map
        (l (a)
          ( (if (or (string-prefix? "./" a) (string-prefix? "../" a) (string-prefix? "/" a))
              cp-include-path cp-include)
            a))
        paths)
      "\n"))

  (define (sc-pre-include-variable name)
    "symbol -> symbol
     return a name for a preprocessor variable marking a file as having been included.
     #define sc_included_{name}"
    (string->symbol (string-append "sc_included_" (symbol->string name))))

  (define (sc-pre-include-define name)
    "symbol -> string
     define a preprocessor variable for marking a file as having been included"
    (list (q pre-define) (sc-pre-include-variable name)))

  (define (sc-enum-entries a) "list -> string"
    (string-join
      (map
        (l (e)
          (match e ((name value) (string-append (sc-identifier name) "=" (sc-value value)))
            (name (sc-identifier name))))
        a)
      ","))

  (define (sc-enum a)
    (let (c (l (name entries) (c-statement (string-append "enum" name) (sc-enum-entries entries))))
      (match a ((name (entries ...)) (c (string-append " " (sc-identifier name)) entries))
        (((entries ...)) (c "" entries)))))

  (define (sc-struct-or-union-body elements compile)
    (string-join
      (map
        (l (a)
          (match a
            ( (name type (? integer? bits))
              (string-append
                (string-join (map (l (a) (sc-compile-type a compile)) type) " " (q suffix))
                (sc-identifier name) ":" (sc-value bits)))
            ( (name type)
              (if (sc-function-pointer? type)
                (apply sc-function-pointer compile (compile name) (tail type))
                (match type (((quote array) a ...) (sc-define-array (pair name a) compile))
                  (else (string-append (sc-compile-type type compile) " " (compile name))))))))
        elements)
      ";" (q suffix)))

  (define sc-included-paths (ht-create-string))

  (define (sc-path->full-path load-paths path) "expects load paths to have a trailing slash"
    (let* ((path (string-append path ".sc")) (path-found (search-load-path path load-paths)))
      (if path-found (realpath* path-found)
        (raise
          (list (q file-not-accessible)
            (string-append (any->string path) " not found in " (any->string load-paths)))))))

  (define (sc-include-sc-once state paths compile->sc) "(string ...) (symbol/string ...) -> list"
    (compile->sc
      (pair (q begin)
        (map
          (l (path)
            (let (path (sc-path->full-path (sc-state-load-paths state) path))
              (if (ht-ref sc-included-paths path) (q (begin))
                (begin (ht-set! sc-included-paths path #t) (pairs (q begin) (file->datums path))))))
          paths))))

  (define (sc-include-sc state paths compile->sc) "(string ...) (string ...) -> list"
    (compile->sc
      (pair (q begin)
        (append-map
          (l (a) (let (a (sc-path->full-path (sc-state-load-paths state) a)) (file->datums a read)))
          paths))))

  (define (sc-define-array a compile)
    (match a
      ( (name type size values ...)
        (let (size (any->list size))
          (c-define-array (compile name) (compile type)
            (if (null? size) (list "") (map compile size))
            (if (null? values) #f (map compile values)))))))

  (define (sc-define a compile)
    "(argument ...) procedure -> string/false
     if the first argument is a preprocessor command, it is a variable declaration"
    (match a
      ( ( ( (? not-preprocessor-keyword? name) parameter ...)
          ((? not-function-pointer-symbol? return-type) types ...) body ...)
        (sc-function compile name return-type body parameter types))
      ( ( ( (? not-preprocessor-keyword? name)) return-type body ...)
        (sc-function compile name return-type body null null))
      ( (name type value rest ...)
        (string-join
          (map-slice 3
            (l (name type value)
              (c-define (compile name) (sc-compile-type type compile) (compile value)))
            a)
          ";"))
      (_ #f)))

  (define (sc-struct-or-union keyword a compile) "symbol/false ? procedure -> string"
    (let (keyword-string (symbol->string keyword))
      (apply (l (name body) (c-statement name (sc-struct-or-union-body body compile)))
        (if (symbol? (first a))
          (list (string-append keyword-string " " (sc-identifier (first a))) (tail a))
          (list keyword-string a)))))

  (define (sc-declare-variable name type compile)
    (if (sc-function-pointer? type) (apply sc-function-pointer compile (compile name) (tail type))
      (c-variable (compile name) (sc-compile-type type compile))))

  (define (sc-struct-literal a compile)
    (c-compound (map (l (a) (if (list? a) (map compile a) (compile a))) a)))

  (define (sc-declare a compile)
    (sc-join-expressions
      (map-slice 2
        (l (id type)
          (match type
            ( ( (quote struct-variable) type a ...)
              (sc-define (list id type (pair (q struct-literal) a)) compile))
            (((quote array) a ...) (sc-define-array (pair id a) compile))
            (((quote enum) a ...) (sc-enum a))
            ( ( (or (quote struct) (quote union)) (not (? symbol?)) _ ...)
              (sc-struct-or-union (first type) (pair id (tail type)) compile))
            (((quote type) type) (sc-define-type compile id type))
            (_ (or (sc-define (list id type) compile) (sc-declare-variable id type compile)))))
        a)))

  (define* (sc-set a compile #:optional (operator "="))
    (match a ((name value) (c-set (compile name) (compile value) operator)) (_ #f)))

  (define* (sc-set-multiple a compile #:optional (set (q set)))
    "-> list:sc
     the -multiple version gives the expression joiner a chance to choose the right delimiter in context"
    (match a
      ((name-1 value-1 name-2 value-2 rest ...) (pair (q begin) (map-slice 2 (l a (pair set a)) a)))
      (_ #f)))

  (define (sc-cond* a compile)
    "-> list
     expands to if* expressions"
    (let (conditions (reverse a))
      (fold
        (l (condition alternate)
          (list (q if*) (first condition) (add-begin-if-multiple (tail condition)) alternate))
        (match (first conditions) (((quote else) body ...) (add-begin-if-multiple body))
          ((test consequent ...) (list (q if*) test (add-begin-if-multiple consequent))))
        (tail conditions))))

  (define* (sc-pre-cond-if if-type add-endif test consequent #:optional alternate)
    (string-replace-string
      (string-append "#"
        (case if-type ((elif) "elif") ((if) "if") ((ifdef) "ifdef") ((ifndef) "ifndef")) " "
        test "\n"
        consequent "\n"
        (if alternate (string-append "#else\n" alternate "\n") "") (if add-endif "#endif" ""))
      "\n\n" "\n"))

  (define (sc-pre-cond if-type a compile)
    (match a
      ( (only-cond)
        (sc-pre-cond-if if-type #t
          (compile (first only-cond)) (compile (pair (q begin) (tail only-cond)))))
      ( (first-cond middle-cond ... last-cond)
        (string-append
          (sc-pre-cond-if if-type #f
            (compile (first first-cond)) (compile (pair (q begin) (tail first-cond))))
          ; middle-cond
          (apply string-append
            (map
              (l (a)
                (sc-pre-cond-if (q elif) #f (compile (first a)) (compile (pair (q begin) (tail a)))))
              middle-cond))
          ; last-cond
          (let
            ( (test (compile (first last-cond)))
              (consequent (compile (pair (q begin) (tail last-cond)))))
            (if (eq? (q else) (first last-cond)) (string-append "#else\n" consequent "\n#endif")
              (sc-pre-cond-if (q elif) #t test consequent)))))))

  (define (sc-cond a compile)
    (match a
      ( (only-cond)
        (c-if-statement (compile (first only-cond)) (compile (pair (q begin) (tail only-cond)))))
      ( (first-cond middle-cond ... last-cond)
        (string-append
          (c-if-statement (compile (first first-cond)) (compile (pair (q begin) (tail first-cond))))
          (apply string-append
            (map
              (l (a)
                (string-append "else "
                  (c-if-statement (compile (first a)) (compile (pair (q begin) (tail a))))))
              middle-cond))
          (let
            ( (test (compile (first last-cond)))
              (consequent (compile (pair (q begin) (tail last-cond)))))
            (string-append "else"
              (if (eq? (q else) (first last-cond)) (string-append "{" consequent "}")
                (string-append " " (c-if-statement test consequent)))))))))

  (define (sc-while a compile)
    (match a
      ( (test body ...)
        (string-append "while" (parenthesise (compile test))
          (c-compound (compile (pair (q begin) body))))))))
