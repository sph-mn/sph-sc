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
    sc-function
    sc-function-pointer
    sc-function-pointer?
    sc-identifier
    sc-identifier-list
    sc-join-expressions
    sc-pre-include
    sc-pre-include-once
    sc-value
    scp-if
    translate-identifier)
  (import
    (ice-9 match)
    (ice-9 regex)
    (rnrs base)
    (sph)
    (sph lang c expressions)
    (only (guile)
      string-prefix?
      string-suffix?
      negate
      make-regexp
      string-join)
    (only (sph alist) alist)
    (only (sph list) map-slice)
    (only (sph one) alist->regexp-match-replacements)
    (only (sph string)
      parenthesise
      any->string
      regexp-match-replace))

  (define (preprocessor-keyword? a) (and (symbol? a) (string-prefix? "pre-" (symbol->string a))))
  (define not-preprocessor-keyword? (negate preprocessor-keyword?))

  (define (add-begin a)
    (if (and (list? a) (not (null? a)) (equal? (q begin) (first a))) a (list (q begin) a)))

  (define identifier-replacements
    (alist->regexp-match-replacements
      ;(regexp search-string . replacement)
      ;replaced in order
      (alist "->" "_to_"
        ".-" (pair "-" "_")
        ".!$" (pair "!" "_x") "\\?" "_p" ".\\+." (pair "+" "_and_") "./." (pair "/" "_or_"))))

  (define (sc-apply proc a) (c-apply (sc-identifier proc) (string-join (map sc-identifier a) ",")))

  (define (sc-join-expressions a)
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
        (list) a)
      ""))

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

  (define (sc-function compile name return-type body parameter-names parameter-types)
    (let
      ( (body (if (null? body) "" (string-append "{" (sc-join-expressions (map compile body)) "}")))
        (parameters (sc-function-parameters compile parameter-names parameter-types name))
        (name (compile name)))
      (if (sc-function-pointer? return-type)
        (string-append
          (apply sc-function-pointer compile (string-append name parameters) (tail return-type)) body)
        (string-append (sc-compile-type return-type compile) " " name parameters body))))

  (define (sc-function-parameter compile name type)
    (if (sc-function-pointer? type) (apply sc-function-pointer compile (compile name) (tail type))
      (string-append (sc-compile-type type compile) " " (compile name))))

  (define (sc-function-parameters compile names types function-name)
    (parenthesise
      (if (list? names)
        (if (equal? (length names) (length types))
          (string-join (map (l a (apply sc-function-parameter compile a)) names types) ",")
          (throw (q type-and-parameter-list-length-mismatch) function-name names))
        (if (or (symbol? names) (string? names))
          (string-append (compile types) " " (compile names))
          (throw (q cannot-convert-to-c-parameter))))))

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

  (define (translate-identifier a) (regexp-match-replace a identifier-replacements)))
