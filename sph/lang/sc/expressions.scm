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
    (only (sph one) regexp-match-replace))

  (define (add-begin arg)
    (if (and (list? arg) (not (null? arg)) (equal? (q begin) (first arg))) arg (list (q begin) arg)))

  (define identifier-replacements
    (alist->regexp-match-replacements
      (alist
        ;(regexp search-string . replacement)
        "->" "_to_"
        ".-" (pair "-" "_") "\\?" "_p" ".!$" (pair "!" "_x") ".\\+." (pair "+" "_and_"))))

  (define (sc-apply proc args)
    (c-apply-nc (sc-identifier proc) (string-join (map sc-identifier args) ",")))

  (define* (sc-define name type #:optional value) "any [any] -> string"
    (c-define-nc (sc-identifier name) (sc-identifier type) (if value (sc-value value) value)))

  (define (sc-identifier arg)
    (if (symbol? arg) (translate-identifier (symbol->string arg))
      (if (list? arg) (string-join (map sc-identifier arg) " ") arg)))

  (define (sc-identifier-list arg)
    (string-append "(" (string-join (map sc-identifier arg) ",") ")"))

  (define (sc-value arg)
    (cond ((symbol? arg) (translate-identifier (symbol->string arg)))
      ((and (integer? arg) (<= 0 arg)) (string-append (number->string arg) "u"))
      ((boolean? arg) (if arg "1u" "0u")) (else (c-value arg))))

  (define (scp-if type arg compile)
    (match arg
      ( (test consequent alternate)
        (cp-if type (compile test) (compile (add-begin consequent)) (compile (add-begin alternate))))
      ((test consequent) (cp-if type (compile test) (compile (add-begin consequent)) #f))))

  (define (translate-identifier a) (regexp-match-replace a identifier-replacements)))