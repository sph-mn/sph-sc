(library (sph lang sc expressions)
  (export
    not-preprocessor-keyword?
    preprocessor-keyword?
    sc-apply
    sc-case
    sc-compile-type
    sc-define
    sc-identifier
    sc-identifier-list
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
      negate
      make-regexp
      string-join)
    (only (sph alist) alist)
    (only (sph list) map-slice)
    (only (sph one) alist->regexp-match-replacements)
    (only (sph string) any->string regexp-match-replace))

  (define (preprocessor-keyword? a) (string-prefix? "pre-" (symbol->string a)))
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

  (define (sc-apply proc a)
    (c-apply-nc (sc-identifier proc) (string-join (map sc-identifier a) ",")))

  (define* (sc-define name type #:optional value) "any [any] -> string"
    (c-define-nc (sc-identifier name) (sc-identifier type) (if value (sc-value value) value)))

  (define (sc-identifier a)
    (if (symbol? a) (translate-identifier (symbol->string a))
      (if (list? a) (string-join (map sc-identifier a) " ") (any->string a))))

  (define (sc-compile-type a compile)
    (match a
      ( ( (? symbol? prefix) expr _ ...)
        (if (preprocessor-keyword? (first a)) (compile a) (sc-identifier a)))
      (_ (sc-identifier a))))

  (define (sc-identifier-list a) (string-append "(" (string-join (map sc-identifier a) ",") ")"))

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
              (string-append (sc-pre-include (list path)) (cp-pre-define-nc variable-name "" "")))))
        names/paths)
      "\n"))

  (define (translate-identifier a) (regexp-match-replace a identifier-replacements)))
