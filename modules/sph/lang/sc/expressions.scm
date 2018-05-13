(library (sph lang sc expressions)
  (export
    not-function-pointer-symbol?
    not-preprocessor-keyword?
    preprocessor-keyword?
    sc-apply
    sc-case
    sc-compile-type
    sc-compile-types
    sc-define-function
    sc-define-type
    sc-enum
    sc-function
    sc-function-pointer
    sc-function-pointer?
    sc-identifier
    sc-identifier-list
    sc-include-sc
    sc-include-sc-once
    sc-join-expressions
    sc-macro-function
    sc-pre-include
    sc-pre-include-define
    sc-pre-include-once
    sc-pre-include-variable
    sc-struct-or-union-body
    sc-value
    scp-if
    translate-identifier)
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
      string-prefix?
      string-null?
      string-trim-right
      string-suffix?
      negate
      make-regexp
      string-join)
    (only (sph two) alist->regexp-match-replacements))

  (define (preprocessor-keyword? a) (and (symbol? a) (string-prefix? "pre-" (symbol->string a))))
  (define not-preprocessor-keyword? (negate preprocessor-keyword?))
  (define (not-function-pointer-symbol? a) (not (and (symbol? a) (eq? (q function-pointer) a))))

  (define (add-begin a)
    (if (and (list? a) (not (null? a)) (equal? (q begin) (first a))) a (list (q begin) a)))

  (define identifier-replacements
    (alist->regexp-match-replacements
      ;(regexp search-string . replacement)
      ;replaced in order
      ;mostly equivalent to the c identifier conversion rules used in guile
      ;https://www.gnu.org/software/guile/manual/html_node/API-Overview.html#API-Overview
      (alist "->" "_to_"
        ".-" (pair "-" "_")
        ".!" (pair "!" "_x")
        "\\?" "_p"
        ".\\+." (pair "+" "_and_")
        "./." (pair "/" "_or_")
        ".<" (pair "<" "_less")
        ".>" (pair ">" "_gr") ".<=" (pair "<=" "_leq") ".>=" (pair ">=" "_geq"))))

  (define (sc-apply proc a) (c-apply (sc-identifier proc) (string-join (map sc-identifier a) ",")))

  (define* (sc-join-expressions a #:optional (expression-separator ""))
    "main procedure for the concatenation of toplevel expressions. adds semicolons"
    (string-join
      (fold-right
        (l (b prev)
          (pair
            ; preprocessor directives need to start on a separate line
            (cond
              ( (string-prefix? "#" b)
                (if (or (null? prev) (not (string-prefix? "\n" (first prev))))
                  (string-append "\n" b "\n") (string-append "\n" b)))
              ((string-prefix? "\n#" b) b)
              (else
                (cond
                  ((or (string-suffix? ";" b) (string-suffix? "*/" b) (string-suffix? "*/\n" b)) b)
                  ((string-suffix? ":" b) (string-append b "\n")) (else (string-append b ";")))))
            prev))
        (list) (remove string-null? a))
      expression-separator))

  (define* (sc-define-function compile name type #:optional value) "any [any] -> string"
    (let ((name (compile name)) (value (if value (compile value) value)))
      (if (sc-function-pointer? type)
        (let (r (apply sc-function-pointer compile name (tail type))) (if value (c-set r value) r))
        (c-define name (sc-compile-type type compile) value))))

  (define* (sc-define-type compile name value) "any any -> string"
    (let (name (compile name))
      (if (sc-function-pointer? value)
        (string-append "typedef " (apply sc-function-pointer compile name (tail value)))
        (c-typedef name (compile value)))))

  (define (sc-identifier a)
    (if (symbol? a) (translate-identifier (symbol->string a))
      (if (list? a) (string-join (map sc-identifier a) " ") (any->string a))))

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
    (if (sc-function-pointer? type-output)
      (apply sc-function-pointer compile
        (string-append (parenthesise (string-append "*" inner))
          (sc-compile-types type-input compile))
        (tail type-output))
      (string-append (sc-compile-type type-output compile) (parenthesise (string-append "*" inner))
        (sc-compile-types type-input compile))))

  (define (get-body-and-docstring& body compile macro-function? c)
    "list procedure:{string:docstring string:body -> any} -> any"
    (if (null? body) (c #f "")
      (apply
        (l (docstring body)
          (c docstring
            (if macro-function?
              (string-trim-right (sc-join-expressions (map compile body) "\\\n  ") #\;)
              (string-append "{" (sc-join-expressions (map compile body)) "}"))))
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
          (string-append (or docstring "")
            (if (sc-function-pointer? return-type)
              (string-append
                (apply sc-function-pointer compile
                  (string-append name parameters) (tail return-type))
                body-string)
              (string-append (sc-compile-type return-type compile) " " name parameters body-string)))))))

  (define (sc-macro-function name parameter body compile)
    (get-body-and-docstring& body compile
      #t
      (l (docstring body-string)
        (string-append (or docstring "")
          (cp-pre-define (sc-identifier name) body-string (sc-identifier-list parameter))
          (if docstring "\n" "")))))

  (define (sc-function-parameter compile name type)
    (if (sc-function-pointer? type) (apply sc-function-pointer compile (compile name) (tail type))
      (string-append (sc-compile-type type compile) " " (compile name))))

  (define (sc-function-parameters compile names types function-name)
    (parenthesise
      (if (list? names)
        (if (equal? (length names) (length types))
          (string-join (map (l a (apply sc-function-parameter compile a)) names types) ",")
          (raise (list (q type-and-parameter-list-length-mismatch) function-name names)))
        (if (or (symbol? names) (string? names))
          (string-append (compile types) " " (compile names))
          (raise (q cannot-convert-to-c-parameter))))))

  (define (sc-identifier-list a) (parenthesise (string-join (map sc-identifier a) ",")))

  (define (sc-value a)
    (cond ((symbol? a) (translate-identifier (symbol->string a))) ((boolean? a) (if a "1" "0"))
      (else (c-value a))))

  (define (scp-if type a compile)
    (match a
      ( (test consequent alternate)
        (cp-if type (compile test) (compile (add-begin consequent)) (compile (add-begin alternate))))
      ((test consequent) (cp-if type (compile test) (compile (add-begin consequent)) #f))))

  (define (sc-case cond-name predicate subject . clauses) "cond-name is either cond or cond*"
    (pair cond-name
      (map
        (l (a)
          (match a (((quote else) _ ...) a)
            ( ( (objects ...) body ...)
              (pair (pair (q or) (map (l (b) (list predicate b subject)) objects)) body))
            ((object body ...) (pair (list predicate object subject) body))))
        clauses)))

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
      "\n" (q suffix)))

  (define (sc-pre-include-variable name)
    "symbol -> symbol
     return a name for a preprocessor variable marking a file as having been included.
     #define sc_included_{name}"
    (string->symbol (string-append "sc_included_" (symbol->string name))))

  (define (sc-pre-include-define name)
    "symbol -> string
     define a preprocessor variable for marking a file as having been included"
    (list (q pre-define) (sc-pre-include-variable name)))

  (define (sc-pre-include-once names/paths)
    "([symbol:name string:path] ...) ->
     include a c file as with sc-pre-include but also check and eventually create a preprocessor variable sc_included_{name}
     that specifies if the file has already been included. wrap the include in a #ifndef.
     unfortunately it seems too difficult to automatically create identifiers sfor included files, therefore they need to be specified"
    (string-join
      (map-slice 2
        (l (name path)
          (let (variable-name (sc-identifier (sc-pre-include-variable name)))
            (cp-if (q ifndef) variable-name
              (string-append (sc-pre-include (list path)) (cp-pre-define variable-name "" "")))))
        names/paths)
      "\n"))

  (define (translate-identifier a) (regexp-match-replace a identifier-replacements))

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
                (string-append (sc-compile-type type compile) " " (compile name))))))
        elements)
      ";" (q suffix)))

  (define (sc-path->full-path load-paths path)
    (let* ((path (string-append path ".sc")) (path-found (search-load-path path load-paths)))
      (if path-found path-found
        (raise
          (list (q file-not-accessible)
            (string-append (any->string path) " not found in " (any->string load-paths)))))))

  (define sc-included-paths (ht-create-string))

  (define (sc-include-sc-once load-paths name/path)
    "(string ...) (symbol/string ...) -> list
     pre-include-once should be preferred for modules that are available to other code because
     sc can not prevent repeated inclusion with c files that were built using sc-include-sc"
    (pair (q begin)
      (map-slice 2
        (l (name path)
          (let
            ( (path (sc-path->full-path load-paths path))
              (variable-name (sc-pre-include-variable name)))
            (if (ht-ref sc-included-paths path) (q (begin))
              (begin (ht-set! sc-included-paths path #t)
                (list (q pre-if-not-defined) variable-name
                  (pairs (q begin) (sc-pre-include-define name) (file->datums path)))))))
        name/path)))

  (define (sc-include-sc load-paths paths) "(string ...) (string ...) -> list"
    (pair (q begin)
      (append-map (l (a) (let (a (sc-path->full-path load-paths a)) (file->datums a read))) paths))))
