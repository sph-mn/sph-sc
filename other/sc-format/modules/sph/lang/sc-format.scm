(library (sph lang sc-format)
  (export sc-format)
  (import
    (ice-9 match)
    (sph)
    (sph hashtable)
    (sph lang scm-format)
    (sph lang scm-format format)
    (sph string)
    (only (guile) inf string-join)
    (only (sph list) map-slice))

  (define sph-lang-sc-format-description
    "formatter for sc (sph-sc) source code. also serves as an example of how custom formatters can be defined")

  (define-syntax-rule (map-recurse recurse a indent) (map (l (a) (first (recurse a indent))) a))
  (define sc-f format-list-f)

  (define (format-define a recurse config indent)
    (match a ((_ (name ...) types body) ((sc-f 3 1 0) a recurse config indent))
      ((_ (name ...) types body ...) ((sc-f 3 1 1) a recurse config indent))
      ((_ name type value) ((sc-f 4 1 0) a recurse config indent))
      ((_ name type) ((sc-f 4 1 0) a recurse config indent))
      ((_ name/type ...) ((sc-f 1 2 2) a recurse config indent))))

  (define (format-set a recurse config indent)
    (match a ((_ name value) ((sc-f 2 1 0) a recurse config indent))
      ((_ name/type ...) ((sc-f 1 2 2) a recurse config indent))
      ; todo: consider comments
      #;( (_ name/type ...)
        (let*
          ( (max (ht-ref-q config max-chars-per-line))
            (indent-string (ht-ref-q config indent-string))
            (indent-length (* indent (string-length indent-string))))
          (list
            (string-join
              (map-slice 2
                (l (b c)
                  (let*
                    ( (b-string (any->string (first (recurse b indent))))
                      (c-string (any->string (first (recurse c indent))))
                      (line-length
                        (+ 1 indent-length (string-length b-string) (string-length c-string))))
                    (apply string-append indent-string
                      b-string
                      (if (< max line-length) (list "\n" indent-string c-string)
                        (list " " c-string)))))
                name/type)
              "\n")
            #f)))))

  #;(define (format-begin a recurse config indent)
    (match a ((_ name value) ((sc-f 2 0 0) a recurse config indent))
      (_ ((sc-f 1 1 1) a recurse config indent))))

  (define-as sc-format-default-config ht-create-symbol-q
    descend-prefix->format-f
    (ht-create-symbol-q
      ; (sc-f 1 1 0) is the default
      begin (sc-f 1 1 1)
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
      struct-pointer-set (sc-f 2 2 2)
      struct-set (sc-f 2 2 2) array-set (sc-f 2 2 2) while (sc-f 2 1 1))
    format
    (ht-create-symbol-q indent-string (string-multiply " " 2)
      max-chars-per-line 100
      max-exprs-per-line-start 3
      max-exprs-per-line-middle 3
      max-exprs-per-line-end (inf)
      docstring-offset-doublequote #t
      multiple-leading-parenthesis-spacing #t
      toplevel-vertical-spacing 1 toplevel-vertical-spacing-oneline 0))

  (define* (sc-format a #:key (indent 0) (config sc-format-default-config))
    (let
      ( (result (map (l (a) (first (scm-format-list->string a indent config))) a))
        (config-format (ht-tree-ref config (q format))))
      (string-join-with-vertical-spacing result ""
        (ht-ref config-format (q toplevel-vertical-spacing))
        (ht-ref config-format (q toplevel-vertical-spacing-oneline))))))
