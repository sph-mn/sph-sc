(library (sph lang sc expressions)
  (export
    sc-apply
    sc-define
    sc-identifier
    sc-identifier-list
    sc-value
    scp-if
    translate-identifier)
  (import
    (ice-9 match)
    (ice-9 regex)
    (rnrs base)
    (sph)
    (sph lang c expressions)
    (only (guile) make-regexp string-join)
    (only (sph alist) alist)
    (only (sph one) alist->regexp-match-replacements)
    (only (sph string) regexp-match-replace))

  (define (add-begin arg)
    (if (and (list? arg) (not (null? arg)) (equal? (q begin) (first arg))) arg (list (q begin) arg)))

  (define identifier-replacements
    (alist->regexp-match-replacements
      ;(regexp search-string . replacement)
      ;replaced in order
      (alist "->" "_to_"
        ".-" (pair "-" "_")
        ".!$" (pair "!" "_x") "\\?" "_p" ".\\+." (pair "+" "_and_") "./." (pair "/" "_or_"))))

  (define (sc-apply proc args)
    (c-apply-nc (sc-identifier proc) (string-join (map sc-identifier args) ",")))

  (define* (sc-define name type #:optional value) "any [any] -> string"
    (c-define-nc (sc-identifier name) (sc-identifier type) (if value (sc-value value) value)))

  (define (sc-identifier a)
    (if (symbol? a) (translate-identifier (symbol->string a))
      (if (list? a) (string-join (map sc-identifier a) " ") a)))

  (define (sc-identifier-list a) (string-append "(" (string-join (map sc-identifier a) ",") ")"))

  (define (sc-value a)
    (cond ((symbol? a) (translate-identifier (symbol->string a)))
      ((and (integer? a) (<= 0 a)) (string-append (number->string a) "u"))
      ((boolean? a) (if a "1u" "0u")) (else (c-value a))))

  (define (scp-if type a compile)
    (match a
      ( (test consequent alternate)
        (cp-if type (compile test) (compile (add-begin consequent)) (compile (add-begin alternate))))
      ((test consequent) (cp-if type (compile test) (compile (add-begin consequent)) #f))))

  (define (translate-identifier a) (regexp-match-replace a identifier-replacements)))