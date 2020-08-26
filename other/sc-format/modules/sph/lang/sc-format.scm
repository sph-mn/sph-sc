(define-module (sph lang sc-format))

(use-modules ((srfi srfi-1) #:select (split-at)) (ice-9 match)
  (sph) (sph hashtable)
  (sph lang scm-format) (sph lang scm-format format) (sph string) ((sph list) #:select (map-slice)))

(export sc-format sc-f scd-f sc-format-config sc-formatters)

(define sph-lang-sc-format-description
  "formatter for sc (sph-sc) source code. also serves as an example of how custom formatters can be defined")

(define (map-recurse recurse a indent) (map (l (a) (first (recurse a indent))) a))
(define sc-f format-list-f)

(define (sc-docstring a recurse config indent)
  (if (null? a) a
    (if (string? (first a))
      (pair
        (format-docstring (first a) (ht-ref-q config docstring-offset-doublequote)
          (ht-ref-q config indent-string) indent)
        (map-recurse recurse (tail a) indent))
      (map-recurse recurse a indent))))

(define (scd-f start mid end docstring-offset)
  (l (a recurse config indent)
    (list
      (format-list
        (apply-values
          (l (prefix suffix) (append prefix (sc-docstring suffix recurse config indent)))
          (split-at a docstring-offset))
        config indent start mid end)
      #f)))

(define (format-define a recurse config indent)
  (match a ((_ (name ...) types body) ((scd-f 3 1 0 3) a recurse config indent))
    ((_ (name ...) types body ...) ((scd-f 3 1 1 3) a recurse config indent))
    ((_ name type value) ((sc-f 4 1 0) a recurse config indent))
    ((_ name type) ((sc-f 4 1 0) a recurse config indent))
    ((_ name/type ...) ((sc-f 1 2 2) a recurse config indent))))

(define (format-set a recurse config indent)
  (let (config (ht-copy* config (l (a) (ht-set-multiple! a (q max-exprs-per-line-assoc) 2))))
    (match a ((_ name value) ((sc-f 2 1 0) a recurse config indent))
      ((_ name/type ...) ((sc-f 1 2 2) a recurse config indent)))))

(define (format-sc-comment a recurse config indent)
  (list
    (format-list
      (pair (first a)
        (map
          (l (a)
            (if (string? a)
              (format-docstring a (ht-ref-q config docstring-offset-doublequote)
                (ht-ref-q config indent-string) indent)
              (first (recurse a indent))))
          (tail a)))
      config indent 1 1 1)
    #f))

"(sc-f 1 1 0) is the default"

(define sc-formatters
  (ht-create-symbol-q sc-define-syntax* (scd-f 2 1 1 2)
    begin (scd-f 1 1 1 1)
    sc-comment format-sc-comment
    for (sc-f 2 1 1)
    case (sc-f 3 1 1)
    case* (sc-f 3 1 1)
    declare format-set
    define format-define
    do-while (sc-f 2 1 1)
    if (sc-f 3 1 1)
    if* (sc-f 3 1 1)
    label (sc-f 2 1 1)
    pre-define format-set
    pre-define-if-not-defined format-set
    pre-include-once (sc-f 1 2 2)
    pre-let format-let
    range-comment format-range-comment
    sc-include-once (sc-f 1 2 2)
    scsh-block-comment format-scsh-block-comment
    semicolon-comment format-semicolon-comment
    set format-set
    struct (sc-f 1 1 1)
    struct-pointer-set (sc-f 2 2 2) struct-set (sc-f 2 2 2) array-set (sc-f 2 2 2) while (sc-f 2 1 1)))

(define sc-format-config
  (ht-create-symbol-q indent-string (string-multiply " " 2)
    max-chars-per-line 100
    max-exprs-per-line-start 3
    max-exprs-per-line-middle 3
    max-exprs-per-line-end (inf)
    max-exprs-per-line-assoc 1
    docstring-offset-doublequote #t
    multiple-leading-parenthesis-spacing #t
    toplevel-vertical-spacing 1 toplevel-vertical-spacing-oneline 0))

(define sc-format-default-config
  (ht-create-symbol-q descend-prefix->format-f sc-formatters format sc-format-config))

(define* (sc-format a #:key (indent 0) (config sc-format-default-config))
  (let
    ( (result (map (l (a) (first (scm-format-list->string a indent config))) a))
      (config-format (ht-tree-ref config (q format))))
    (string-join-with-vertical-spacing result ""
      (ht-ref config-format (q toplevel-vertical-spacing))
      (ht-ref config-format (q toplevel-vertical-spacing-oneline)))))
