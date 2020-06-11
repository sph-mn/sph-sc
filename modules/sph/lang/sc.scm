(define-module (sph lang sc))

(use-modules (srfi srfi-1) ((rnrs io ports) #:select (get-datum))
  (ice-9 match) (sph)
  (sph list)
  ( (sph string) #:select
    (parenthesise parenthesised? any->string
      any->string-write regexp-match-replace regexp-replace string-replace-string string-enclose))
  ( (sph hashtable) #:select
    (ht-create-symbol-q ht-create-symbol ht-delete!
      ht-ref ht-set! ht-from-list ht-hash-symbol ht-create-string))
  ((sph alist) #:select (alist))
  ((sph filesystem) #:select (ensure-trailing-slash search-load-path realpath*)))

(export sc->c sc-default-load-paths
  sph-lang-sc-description sc-syntax-table sc-syntax-check sc-syntax-error sc-syntax-error?)

(define sph-lang-sc-description
  "an s-expression to c compiler.
   * syntax checks and usage example for forms on error
   * parsed using scheme read (scheme comments discarded), resulting nested list of expressions
     is traversed top to bottom and elements are converted to strings
   * identifier replacements see https://www.gnu.org/software/guile/manual/html_node/API-Overview.html#API-Overview
   * prefixes
     * c-: takes strings or lists of strings and returns strings
     * sc-: takes sc or strings and returns strings")

(define (vector-accessor index)
  "integer -> procedure:{vector -> any}
   return a procedure that when called with a vector returns the value at index"
  (l (a) (vector-ref a index)))

(define (tree-finder find f a)
  "procedure:{predicate list ... -> any} procedure:{element -> any/boolean} list -> any
   call f with each tree list and leaf from top to bottom and return true results of f"
  (find (l (a) (or (f a) (and (list? a) (tree-finder find f a)))) a))

(define (tree-any f a)
  "procedure list -> false/any
   call f with each tree element list or leaf from top to bottom and return the first true result of f.
   can be used to extract single elements from tree. aliased as tree-any and tree-extract"
  (tree-finder any f a))

(define (tree-every f a) (not (tree-any (negate f) a)))

(define* (tree-contains? a search-value #:optional (equal? equal?))
  "list any [procedure:{any any -> boolean}] -> boolean
   compares all list and non-list elements with search-value and returns true on a match"
  (any (l (a) (or (equal? a search-value) (and (list? a) (tree-contains? a search-value)))) a))

(define* (port->datums port #:optional (get-datum get-datum))
  "string procedure:reader -> list
   read all scheme datums from a port"
  (let loop ((a (get-datum port))) (if (eof-object? a) (list) (pair a (loop (get-datum port))))))

(define* (file->datums path #:optional (get-datum get-datum))
  "string procedure:reader -> list
   read all scheme datums of a file specified by path"
  (call-with-input-file path (l (port) (port->datums port get-datum))))

(define sc-state-load-paths (vector-accessor 1))
(define sc-state-no-semicolon (vector-accessor 2))
(define ambiguous-regexp (make-regexp "^(\\*|&)+|\\.|->|\\[|\\("))
(define (sc-state-new load-paths) (vector (q sc-state) load-paths (ht-create-symbol)))

(define (sc-syntax-examples-get name) "prepend the prefix symbol to example argument patterns"
  "symbol -> false/list"
  (and-let* ((examples (ht-ref sc-syntax-examples name)))
    (map (l (a) (if (list? a) (pair name a) a)) examples)))

(define (sc-syntax-error? a) (and (list? a) (not (null? a)) (eq? (q sc-syntax-error) (first a))))

(define* (sc-syntax-error #:optional irritant syntax-name expected)
  "false/any false/symbol false/(any ...) | exception"
  (raise
    (pair (q sc-syntax-error)
      (compact
        (list (and irritant (pair (q irritant) irritant))
          (or (and expected (pair (q expected) expected))
            (and syntax-name
              (and-let* ((examples (sc-syntax-examples-get syntax-name)))
                (pair (q expected) examples)))))))))

(define* (acount? a min #:optional max)
  (let (b (length a)) (and (if min (<= min b) #t) (if max (>= max b) #t))))

(define sc-syntax-examples
  (ht-from-list
    (list-q address-of ((variable))
      case
      ( (predicate subject (match-value/else consequent ...) ..1)
        (= variable (3 (return #t)) (5 (return #t)) (else (return #f))))
      declare ((name type) (name type name type name/type ...))
      define
      ( (name type value) (name type value name type value ...)
        ((name parameters ...) (return-type ...) body ...)
        ((name parameters ...) return-type body ...))
      for
      ( ( (init test update) body ...) (((init ...) test (update ...)) body ...)
        (((begin init ...) test (begin update ...)) body ...))
      pointer-get ((variable))
      array-get ((variable indices ...))
      convert-type ((variable new-type))
      if ((condition consequent) (condition consequent alternate))
      set ((variable value) (variable value variable value variable/value ...)))
    eq? ht-hash-symbol))

(define (sc-syntax-check-prefix-list prefix a state)
  "symbol list sc-state -> boolean
   arity checks first"
  (case prefix
    ( (+ -* /
        array-get cond
        cond* sc-include
        function-pointer label
        pre-pragma pre-undefine
        pre-include pre-concat pre-string-concat pre-stringify sc-comment while)
      (acount? a 1))
    ( (: struct-pointer-get do-while
        bit-or bit-and bit-xor or and modulo pre-let let let* = < > <= >=)
      (acount? a 2))
    ((array-set struct-set) (acount? a 3))
    ((address-of bit-not goto not pointer-get) (acount? a 1 1))
    ((enum) (acount? a 1 2))
    ((convert-type bit-shift-right bit-shift-left) (acount? a 2 2))
    ((case case*) (match a ((predicate subject clauses ..1) #t) (_ #f)))
    ((declare) (even? (length a)))
    ( (define)
      (match a
        ( ( ( (? not-preprocessor-keyword? name) parameter ...)
            ((? not-function-pointer-symbol? return-type) parameter-types ...) body ...)
          (equal? (length parameter) (length parameter-types)))
        ((((? not-preprocessor-keyword? name)) return-type body ...) #t)
        ((name type value rest ...) (zero? (modulo (length rest) 3))) (_ #f)))
    ((for) (match a (((init test update) body ...) #t) (_ #f)))
    ( (if if* pre-if pre-if-defined pre-if-not-defined)
      (match a ((test consequent) #t) ((test consequent alternate) #t) (_ #f)))
    ( (pre-define pre-define-if-not-defined)
      (or (acount? a 1 1) (and (even? (length a)) (acount? a 2))))
    ( (set)
      (and (even? (length a))
        (match a ((name-1 value-1 name-2 value-2 rest ...) #t) ((name value) #t) (_ #f))))
    ( (struct union)
      (match a (((? symbol?) (name type ...) ...) #t) (((name type ...) ...) #t) (_ #f)))
    (else #t)))

(define (sc-syntax-check a state) "list:expressions (string ...) -> boolean | exception"
  (tree-every
    (l (a)
      (if (and (list? a) (not (null? a)))
        (or (sc-syntax-check-prefix-list (first a) (tail a) state) (sc-syntax-error a (first a))) #t))
    a))

(define (parenthesise-ambiguous a)
  (if (or (parenthesised? a) (not (regexp-exec ambiguous-regexp a))) a (parenthesise a)))

(define (parenthesise-ensure a) (if (parenthesised? a) a (parenthesise a)))
(define (c-comma-join a) (string-join a ","))
(define (c-curly-brackets a) (string-append "{" a "}"))
(define (cp-undef a) (string-append "#undef " a))
(define (cp-include-path path) (string-append "#include " (c-string path)))
(define (cp-include path) (string-append "#include <" path ">"))
(define (cp-concat a) "(string ...) -> string" (string-join a "##"))
(define (cp-stringify a) "string -> string" (string-append "#" a))

(define* (cp-define name #:optional body parameters) "string false/string false/string -> string"
  (string-append "#define " name
    (if parameters parameters "") (if body (string-append " " body) "")))

(define* (cp-if type test consequent #:optional alternate)
  (string-replace-string
    (string-append "#"
      (case type
        ((if) "if")
        ((ifdef) "ifdef")
        ((ifndef) "ifndef")
        (else (raise (list (q sc-syntax-error) (q pre-if) type test consequent alternate))))
      " " test
      "\n" consequent "\n" (if alternate (string-append "#else\n" alternate "\n") "") "#endif")
    "\n\n" "\n"))

(define (c-compound a)
  "string/((string string) ...) -> string
   a: {a}
   (a): {string}
   (a b): {a,b}
   ((a b) (c d)): {.a=b,.c=d}
   ((a b) c): {.a=b,c}
   also creates compound literals"
  (string-append "{"
    (if (list? a)
      (string-join (map (l (a) (if (list? a) (string-append "." (first a) "=" (second a)) a)) a)
        ",")
      a)
    "}"))

(define (c-for init test update body) (string-append "for(" init ";" test ";" update "){" body "}"))
(define (c-typedef name a) (string-append "typedef " a " " name))

(define (c-convert-type a type)
  "extra round brackets ensure nestability in function pointer cases like this: (dg_pair_reader_t)((*state).reader)(state,count,result)"
  (string-append "((" type ")(" a "))"))

(define* (c-statement keyword body #:optional prefix-a suffix-a)
  "\"keyword (prefix-a) { body } (suffix-a)\""
  (string-append keyword (if prefix-a (parenthesise prefix-a) "")
    (c-compound body) (if suffix-a (parenthesise suffix-a) "")))

(define (c-enum name enum-list)
  (string-append "enum " name
    (c-compound
      (string-join
        (map (l (e) (if (list? e) (string-append (first e) "=" (first (tail e))) e)) enum-list) ","))))

(define (c-define-array name type sizes values) "string string (string ...) string ... -> string"
  (string-append type " "
    name (apply string-append (map (l (a) (string-append "[" a "]")) sizes))
    (if values (string-append "={" (string-join values ",") "}") "")))

(define (c-variable name type) "string string -> string" (string-append type " " name))

(define (c-define name type value) "string string [string] -> string"
  (string-append (c-variable name type) (if value (string-append "=" value) "")))

(define (c-identifier a)
  (string-append
    (cond
      ((symbol? a) (symbol->string a))
      ((string? a) a)
      (else (throw (q cannot-convert-to-c-identifier))))))

(define (c-identifier-list a)
  (parenthesise
    (if (list? a) (string-join (map c-identifier a) ",")
      (if (or (symbol? a) (string? a)) (c-identifier a) (throw (q cannot-convert-to-c-identifier))))))

(define (c-function-parameter name type) (string-append type " " name))

(define (c-function-parameters names types)
  (parenthesise
    (if (list? names)
      (if (equal? (length names) (length types))
        (string-join (map c-function-parameter names types) ",")
        (throw (q type-and-parameter-list-length-mismatch) names))
      (if (or (symbol? names) (string? names)) (c-function-parameter names types)
        (throw (q cannot-convert-to-c-parameter))))))

(define* (c-function name type-output body #:optional (names (list)) (type-input (list)))
  (string-append (if type-output (string-append type-output " ") "") name
    (catch (q type-and-parameter-list-length-mismatch)
      (nullary (c-function-parameters names type-input)) (l (key . data) (apply throw key name data)))
    (if body (string-append "{" body "}") "")))

(define* (c-apply proc-name #:optional (args ""))
  (string-append (parenthesise-ambiguous proc-name) (parenthesise args)))

(define* (c-if test consequent #:optional alternate)
  "string string [string] -> string
   create an if expression"
  (string-append "(" test "?" consequent ":" (if alternate alternate "0") ")"))

(define* (c-if-statement test consequent #:optional alternate)
  "string string [string] -> string
   create an if statement"
  (string-append "if" (parenthesise-ensure test)
    "{" consequent "}" (if alternate (string-append "else{" alternate "}") "")))

(define (c-array-get a . indices)
  (apply string-append (parenthesise-ambiguous a) (map (l (a) (string-append "[" a "]")) indices)))

(define (c-struct-get a . keys) (string-join (pair (parenthesise-ambiguous a) keys) "."))

(define (c-struct-pointer-get a . fields)
  (string-append (parenthesise-ambiguous a) (string-join fields "->" (q prefix))))

(define (c-pointer-get a) (string-append "*" (parenthesise-ambiguous a)))
(define* (c-set name value #:optional (operator "=")) (string-append name operator value))
(define (c-pointer type) (string-append type " * "))
(define (c-address-of a) (string-append "&" (parenthesise-ambiguous a)))
(define (c-not a) (string-append "!" a))
(define (c-bit-not a) (string-append "~" a))

(define (c-function-pointer inner type-output . type-input)
  (string-append type-output "(*" inner ")(" (string-join type-input ",") ")"))

(define c-escape-single-char
  (alist "\"" "\\\"" "\a" "\\a" "\n" "\\n" "\b" "\\b" "\f" "\\f" "\r" "\\r" "\t" "\\t" "\v" "\\v"))

(define (c-string a)
  (string-enclose
    (fold (l (a result) (string-replace-string result (first a) (tail a))) a c-escape-single-char)
    "\""))

(define (c-value a) "handles the default conversions between scheme and c types"
  (cond
    ((symbol? a) (symbol->string a))
    ((string? a) (c-string a))
    ((number? a) (number->string a))
    ((boolean? a) (if a "1" "0"))
    ( (char? a)
      (let (a (any->string-write (string a)))
        (string-enclose (substring a 1 (- (string-length a) 1)) "'")))
    (else (throw (q cannot-convert-to-c-value) a))))

(define (sc-no-semicolon-register state name)
  "symbol -> unspecified
   dont add semicolon after name when used"
  (ht-set! (sc-state-no-semicolon state) name #t))

(define (sc-no-semicolon-unregister state name) (ht-delete! (sc-state-no-semicolon state) name))
(define (sc-no-semicolon-registered? state name) (ht-ref (sc-state-no-semicolon state) name))

(define (sc-no-semicolon expressions compile state)
  "(sc ...) -> c
   dont add semicolons after expressions. for macros that expand to function definitions whose body must not
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
  "converts strings alist key strings to regular expressions"
  (map (l (e) (pair (if (string? (first e)) (make-regexp (first e)) (first e)) (tail e))) a))

(define identifier-replacements
  (alist->regexp-match-replacements
    (alist "->" "_to_"
      ".-" (pair "-" "_")
      "-." (pair "-" "_")
      ".!" (pair "!" "_x")
      "\\?" "_p"
      "./." (pair "/" "_or_")
      ".<" (pair "<" "_less") ".>" (pair ">" "_gr") ".<=" (pair "<=" "_leq") ".>=" (pair ">=" "_geq"))))

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

(define (sc-let* a compile state)
  (c-compound
    (match a
      ( ( ( (names values ...) ...) body ...)
        (compile
          (pair (q begin)
            (append
              (map (l (n v) (pairs (if (= 1 (length v)) (q set) (q define)) n v)) names values) body)))))))

(define (sc-pre-define-if-not-defined a compile state)
  (compile
    (pair (q begin)
      (map-slice 2
        (l (name value)
          (let (identifier (match name (((? not-preprocessor-keyword? name) _ ...) name) (_ name)))
            (qq
              (pre-if-not-defined (unquote identifier)
                (unquote
                  (if value (qq (pre-define (unquote name) (unquote value)))
                    (qq (pre-define (unquote name)))))))))
        (if (= 1 (length a)) (list (first a) #f) a)))))

(define (sc-do-while a compile state)
  (match a
    ( (test body ...)
      (string-append "do" (c-compound (compile (pair (q begin) body)))
        "while" (parenthesise (compile test))))))

(define (sc-pre-let* a compile state)
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

(define (sc-for a compile state)
  (let
    (comma-join
      (l (a)
        (match a (((quote begin) a ...) (string-join (map compile a) ","))
          (((? symbol?) _ ...) (compile a)) (_ (string-join (map compile a) ",")))))
    (match a
      ( ( (init test update) body ...)
        (c-for (comma-join init) (compile test) (comma-join update) (compile (pair (q begin) body)))))))

(define (sc-if a compile state)
  (match a
    ( (test consequent alternate)
      (c-if-statement (compile test) (compile (list (q begin) consequent))
        (compile (list (q begin) alternate))))
    ((test consequent) (c-if-statement (compile test) (compile (list (q begin) consequent))))))

(define (sc-semicolon-list-to-comma-list a)
  "this would not work with string or character literals that contain semicolons.
   if these literals can occur anywhere other than definitions this needs to be changed"
  (parenthesise (string-replace-string (string-trim-right a #\;) ";" ",")))

(define (sc-if* a compile state)
  (apply c-if
    (map
      (l (b)
        (match b
          ( ( (quote begin) body ...)
            (parenthesise
              (string-join
                (map
                  (l (b)
                    (if (contains-set? b) (sc-semicolon-list-to-comma-list (compile b)) (compile b)))
                  body)
                ",")))
          (_ (if (contains-set? b) (sc-semicolon-list-to-comma-list (compile b)) (compile b)))))
      a)))

(define (sc-apply name a compile state)
  (string-append
    (c-apply (compile name) (string-join (map (compose parenthesise-ambiguous compile) a) ","))
    (if (sc-no-semicolon-registered? state name) "\n" "")))

(define* (sc-join-expressions a #:optional (expression-separator ""))
  "main procedure for the concatenation of toplevel expressions"
  (define (fold-f b prev)
    (pair
      (cond
        ( (string-prefix? "#" b) "preprocessor directives need to be on a separate line"
          (if (and (not (null? prev)) (string-suffix? "\n" (first prev))) (string-append b "\n")
            (string-append "\n" b "\n")))
        ( (string-prefix? "/*" b)
          (if (and (not (null? prev)) (string-suffix? "\n" (first prev))) b (string-append "\n" b)))
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
      (string-join (map compile a) " ") (compile a))
    (sc-identifier a)))

(define (sc-compile-types a compile)
  (parenthesise (string-join (map (l (e) (sc-compile-type e compile)) a) ",")))

(define (sc-function-pointer? a)
  (and (list? a) (not (null? a)) (eq? (q function-pointer) (first a))))

(define (sc-function-pointer compile inner type-output . type-input)
  "procedure string ? ? ... -> string"
  (if (sc-function-pointer? type-output)
    (apply sc-function-pointer compile
      (string-append (parenthesise (string-append "*" inner)) (sc-compile-types type-input compile))
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
              (string-append (sc-compile-type return-type compile) " " name parameters body-string))
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
      (if (or (symbol? names) (string? names)) (string-append (compile types) " " (compile names))
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

(define (sc-pre-define a compile state) "-> string"
  (match a ((name) (cp-define (sc-identifier name)))
    ( (name value)
      (match name
        ( (name parameter ...) (sc-no-semicolon-track state name value)
          (sc-macro-function name parameter (list value) compile))
        (_ (sc-no-semicolon-track state name value)
          (cp-define (sc-identifier name)
            (string-replace-string (string-trim-right (compile value) #\newline) "\n" "\\\n")))))
    ( (name-1 value-1 name-2 value-2 rest ...)
      (compile (pair (q begin) (map-slice 2 (l a (pair (q pre-define) a)) a))))
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

(define (sc-enum a compile state)
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
              (compile name) ":" (sc-value bits)))
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
        (c-define-array (compile name)
          (match type (((? preprocessor-keyword? _) _ ...) (compile type))
            (else (sc-identifier type)))
          (if (null? size) (list "") (map compile size)) (if (null? values) #f (map compile values)))))))

(define (sc-define a compile state)
  "(argument ...) procedure -> string/false
   if the first argument is a preprocessor command, it is a variable declaration.
   it is still possible to construct function names with the preprocessor"
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
  (let
    ( (keyword-string (symbol->string keyword)) (a-first (first a))
      (c (l (name body) (c-statement name (sc-struct-or-union-body body compile)))))
    (if (or (symbol? a-first) (and (list? a-first) (preprocessor-keyword? (first a-first))))
      (c (string-append keyword-string " " (compile a-first)) (tail a)) (c keyword-string a))))

(define (sc-declare-variable name type compile)
  (if (sc-function-pointer? type) (apply sc-function-pointer compile (compile name) (tail type))
    (c-variable (compile name) (sc-compile-type type compile))))

(define (sc-struct-literal a compile state)
  (c-compound (map (l (a) (if (list? a) (map compile a) (compile a))) a)))

(define (sc-declare a compile state)
  (sc-join-expressions
    (map-slice 2
      (l (id type)
        (match type
          ( ( (quote struct-variable) type a ...)
            (sc-define (list id type (pair (q struct-literal) a)) compile state))
          (((quote array) a ...) (sc-define-array (pair id a) compile))
          (((quote enum) a ...) (sc-enum a compile state))
          ( ( (or (quote struct) (quote union)) (not (? symbol?)) _ ...)
            (sc-struct-or-union (first type) (pair id (tail type)) compile))
          (((quote type) type) (sc-define-type compile id type))
          (_ (or (sc-define (list id type) compile state) (sc-declare-variable id type compile)))))
      a)))

(define* (sc-set-f operator)
  (l (a compile state)
    (match a ((name value) (c-set (compile name) (compile value) operator))
      ( (name-1 value-1 name-2 value-2 rest ...)
        (sc-join-expressions
          (map-slice 2 (l (name value) (c-set (compile name) (compile value) operator)) a)))
      (_ #f))))

(define (sc-cond* a compile state)
  (compile
    (let (conditions (reverse a))
      (fold
        (l (condition alternate)
          (list (q if*) (first condition) (add-begin-if-multiple (tail condition)) alternate))
        (match (first conditions) (((quote else) body ...) (add-begin-if-multiple body))
          ((test consequent ...) (list (q if*) test (add-begin-if-multiple consequent))))
        (tail conditions)))))

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
        (apply string-append
          (map
            (l (a)
              (sc-pre-cond-if (q elif) #f (compile (first a)) (compile (pair (q begin) (tail a)))))
            middle-cond))
        (let
          ( (test (compile (first last-cond)))
            (consequent (compile (pair (q begin) (tail last-cond)))))
          (if (eq? (q else) (first last-cond)) (string-append "#else\n" consequent "\n#endif")
            (sc-pre-cond-if (q elif) #t test consequent)))))))

(define (sc-cond a compile state)
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

(define (sc-while a compile state)
  (match a
    ( (test body ...)
      (string-append "while" (parenthesise (compile test))
        (c-compound (compile (pair (q begin) body)))))))

(define (sc-default-load-paths)
  (map ensure-trailing-slash (let (a (getenv "SC_LOAD_PATH")) (if a (string-split a #\:) null))))

(define (sc-array-set a compile state)
  (let (array (first a))
    (compile
      (pair (q begin)
        (map-slice 2 (l (index value) (list (q set) (list (q array-get) array index) value))
          (tail a))))))

(define (sc-case-f is-case*)
  (l (a compile state)
    (compile
      (match a
        ( (predicate subject clauses ..1)
          (pair (if is-case* (q cond*) (q cond))
            (map
              (l (a)
                (match a (((quote else) _ ...) a)
                  ( ( (objects ...) body ...)
                    (pair (pair (q or) (map (l (b) (list predicate b subject)) objects)) body))
                  ((object body ...) (pair (list predicate object subject) body))))
              clauses)))))))

(define (sc-struct-set a compile state)
  (let (struct (first a))
    (compile
      (pair (q begin)
        (map-slice 2 (l (field value) (list (q set) (list (q struct-get) struct field) value))
          (tail a))))))

(define (sc-infix-f c-infix)
  (l (a compile state)
    (parenthesise
      (string-join
        (map
          (l (a) "consider cases like a&&b=c where a lvalue error would occur for b=c"
            (if (contains-set? a) (parenthesise (compile a)) (compile a)))
          a)
        c-infix))))

(define (sc-comparison-infix-f c-infix)
  (l (a compile state)
    (let (operator (if (eq? "=" c-infix) "==" c-infix))
      ( (if (= 2 (length a)) identity parenthesise)
        (string-join
          (map-segments 2 (l (a b) (parenthesise (string-append a operator b)))
            (map (l (a) (if (contains-set? a) (parenthesise (compile a)) (compile a))) a))
          "&&")))))

(define (sc-address-of a compile state) (c-address-of (apply string-append (map compile a))))

(define sc-syntax-table
  (ht-create-symbol-q array-set sc-array-set
    case (sc-case-f #f)
    case* (sc-case-f #t)
    cond* sc-cond*
    pre-define-if-not-defined sc-pre-define-if-not-defined
    pre-define sc-pre-define
    sc-include sc-include-sc
    sc-include-once sc-include-sc-once
    struct-set sc-struct-set
    + (sc-infix-f "+")
    - (sc-infix-f "-")
    * (sc-infix-f "*")
    / (sc-infix-f "/")
    != (sc-comparison-infix-f "!=")
    = (sc-comparison-infix-f "=")
    < (sc-comparison-infix-f "<")
    > (sc-comparison-infix-f ">")
    <= (sc-comparison-infix-f "<=")
    >= (sc-comparison-infix-f ">=")
    and (sc-infix-f "&&")
    bit-and (sc-infix-f "&")
    bit-or (sc-infix-f "|")
    bit-shift-right (sc-infix-f ">>")
    bit-shift-left (sc-infix-f "<<")
    bit-xor (sc-infix-f "^")
    modulo (sc-infix-f "%")
    or (sc-infix-f "||")
    address-of sc-address-of
    array-get (l (a compile state) (apply c-array-get (map compile a)))
    array-literal (l (a compile state) (c-compound (map compile a)))
    begin (l (a compile state) (sc-join-expressions (map compile a)))
    bit-not (l (a compile state) (c-bit-not (compile (first a))))
    compound-statement (l (a compile state) (c-compound (sc-join-expressions (map compile a))))
    cond sc-cond
    convert-type (l (a compile state) (apply c-convert-type (map compile a)))
    declare sc-declare
    define sc-define
    do-while sc-do-while
    enum sc-enum
    for sc-for
    function-pointer (l (a compile state) (apply sc-function-pointer compile "" a))
    goto (l (a compile state) (string-append "goto " (compile (first a))))
    if sc-if
    if* sc-if*
    label
    (l (a compile state)
      (string-append (compile (first a)) ":" (sc-join-expressions (map compile (tail a)))))
    let* sc-let*
    not (l (a compile state) (c-not (compile (first a))))
    pointer-get (l (a compile state) (apply c-pointer-get (map compile a)))
    pre-pragma
    (l (a compile state) (string-append "#pragma " (string-join (map sc-identifier a) " ") "\n"))
    pre-undefine
    (l (a compile state) (string-join (map (compose cp-undef sc-identifier) a) "\n" (q suffix)))
    pre-let* sc-pre-let*
    pre-include (l (a compile state) (sc-pre-include a))
    pre-concat (l (a compile state) (cp-concat (map sc-identifier a)))
    pre-cond (l (a c s) (sc-pre-cond (q if) a c))
    pre-cond-defined (l (a c s) (sc-pre-cond (q ifdef) a c))
    pre-cond-not-defined (l (a c s) (sc-pre-cond (q ifndef) a c))
    pre-if (l (a c s) (sc-pre-if (q if) a c))
    pre-if-defined (l (a c s) (sc-pre-if (q ifdef) a c))
    pre-if-not-defined (l (a c s) (sc-pre-if (q ifndef) a c))
    pre-stringify (l (a c s) (cp-stringify (c (first a))))
    pre-string-concat (l (a c s) (string-join (map c a) " "))
    return (l (a c s) (if (null? a) "return" (sc-apply (q return) a c s)))
    sc-insert (l (a c s) (first a))
    sc-comment (l (a c s) (string-append "/* " (string-join a "\n") " */\n"))
    sc-no-semicolon sc-no-semicolon
    set (sc-set-f "=")
    set+ (sc-set-f "+=")
    set- (sc-set-f "-=")
    set* (sc-set-f "*=")
    set/ (sc-set-f "/=")
    struct (l (a c s) (sc-struct-or-union (q struct) a c))
    union (l (a c s) (sc-struct-or-union (q union) a c))
    struct-get (l (a c s) (apply c-struct-get (map c a)))
    struct-literal sc-struct-literal
    : (l (a c s) (apply c-struct-pointer-get (map c a)))
    struct-pointer-get (l (a c s) (apply c-struct-pointer-get (map c a))) while sc-while))

(define (sc->c* a state) (define (compile a) (sc->c* a state))
  (if (list? a)
    (let* ((f (ht-ref sc-syntax-table (first a))) (b (and f (f (tail a) compile state))))
      (if b (if (list? b) (sc-join-expressions (map compile b)) b)
        (sc-apply (first a) (tail a) compile state)))
    (sc-value a)))

(define (post-process a) (string-trim (regexp-replace a "\n\n+" "\n")))

(define* (sc->c a #:optional load-paths state)
  "expression [(string ...) sc-state] -> string
   load-paths is only used if state is not given or false"
  (let (state (or state (sc-state-new (or load-paths (sc-default-load-paths)))))
    (and (sc-syntax-check (list a) state) (post-process (sc->c* a state)))))
