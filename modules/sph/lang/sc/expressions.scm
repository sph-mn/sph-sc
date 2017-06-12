(library (sph lang sc expressions)
  (export
    not-preprocessor-keyword?
    preprocessor-keyword?
    sc-apply
    sc-case
    sc-compile-type
    sc-compile-types
    sc-define
    sc-define-type
    sc-enum
    sc-function
    sc-function-pointer
    sc-function-pointer?
    sc-identifier
    sc-identifier-list
    sc-join-expressions
    sc-macro-function
    sc-pre-include
    sc-pre-include-once
    sc-value
    scp-if
    translate-identifier)
  (import
    (ice-9 match)
    (ice-9 regex)
    (sph base)
    (sph lang c expressions)
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
    (string-join
      (fold-right
        (l (e prev)
          (pair
            ;preprocessor directives need to start on a separate line
            (if (string-prefix? "#" e)
              (if (or (null? prev) (not (string-prefix? "\n" (first prev))))
                (string-append "\n" e "\n") (string-append "\n" e))
              (if (string-suffix? ";" e) e
                (if (string-suffix? ":" e) (string-append e "\n") (string-append e ";"))))
            prev))
        (list) (remove string-null? a))
      expression-separator))

  (define* (sc-define compile name type #:optional value) "any [any] -> string"
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

  (define (get-body-and-docstring& body compile macro-function? c) "list procedure -> any"
    (if (null? body) (c #f "")
      (apply
        (l (docstring body)
          (c docstring
            (if macro-function?
              (string-trim-right (sc-join-expressions (map compile body) "\\\n  ") #\;)
              (string-append "{" (sc-join-expressions (map compile body)) "}"))))
        (if (string? (first body)) (list (docstring->comment (first body)) (tail body))
          (list #f body)))))

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
          (raise (list (q type-and-parameter-list-length-mismatch) function-name  names)))
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

  (define (sc-pre-include-once names/paths)
    "([symbol string] ...)
    unfortunately it seems too difficult to guess globally identifying names for included files, therefore they have to be specified"
    (string-join
      (map-slice 2
        (l (name path)
          (let* ((name (sc-identifier name)) (variable-name (string-append "sc_included_" name)))
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
        (((entries ...)) (c "" entries))))))
