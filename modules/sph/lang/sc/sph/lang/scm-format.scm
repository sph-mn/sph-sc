(define-module (sph lang sc sph lang scm-format))

(use-modules (ice-9 streams) (rnrs bytevectors)
  (sph lang sc sph) (sph lang sc sph hashtable)
  (sph lang sc sph io) (sph lang sc sph lang scm-format format)
  (sph lang sc sph lang scm-format transform) (sph lang sc sph tree)
  ((srfi srfi-1) #:select (any)) ((sph lang sc sph list) #:select (map-integers))
  (ice-9 textual-ports)
  ((sph lang sc sph string) #:select (any->string any->string-write* string-multiply string-equal?)))

(define sph-lang-scm-format-description "format scheme code")
(define default-format-ascend format-application)

(define scm-format-default-config
  (ht-create-symbol-q format
    (ht-create-symbol-q indent-string (string-multiply " " 2)
      max-chars-per-line 100
      max-exprs-per-line-start 3
      max-exprs-per-line-middle 2
      max-exprs-per-line-end (inf)
      max-exprs-per-line-assoc 1
      use-read-syntax-quote #f
      docstring-offset-doublequote #t
      multiple-leading-parenthesis-spacing #t
      toplevel-vertical-spacing 1 toplevel-vertical-spacing-oneline 0)
    transform
    (ht-create-symbol-q sort-export #t
      sort-import #t sort-definitions #f separate-unexported-definitions #f)))

(define descend-prefix->format-f
  (ht-create-symbol-q case (format-list-f 2 1 1)
    cond (format-list-f 1 1 1)
    define format-lambda
    define* format-lambda
    define-syntax-case format-lambda
    define-syntax-rule format-lambda
    define-test-module format-test-module
    hash-bang format-hash-bang
    lambda format-lambda
    l format-lambda
    let format-let
    let* format-let
    let-macro format-list-assoc
    library format-library
    quasiquote format-quasiquote
    quasisyntax format-quasisyntax
    quote format-quote
    _parsed-hash-comma-comment format-range-comment
    _parsed-scsh-block-comment format-scsh-block-comment
    _parsed-semicolon-comment format-semicolon-comment
    syntax format-syntax unquote format-unquote unsyntax format-unsyntax))

(define ascend-prefix->format-f (ht-create))

(define (config-add-defaults c) "r6rs-hashtable -> r6rs-hashtable"
  (let
    ( (config-format (ht-ref-q c format)) (config-transform (ht-ref-q c transform))
      (default-config-transform (ht-ref-q scm-format-default-config transform))
      (default-config-format (ht-ref-q scm-format-default-config format)))
    (ht-create-symbol-q format
      (if config-format (ht-copy* default-config-format (l (a) (ht-merge! a config-format)))
        default-config-format)
      transform
      (if config-transform
        (ht-copy* default-config-transform (l (a) (ht-merge! a config-transform)))
        default-config-transform))))

(define (format-sequence a a-length ref current-indent config config-format)
  (first
    (scm-format-list->string (map-integers a-length (l (index) (ref a index))) current-indent
      config)))

(define (format-non-list a current-indent config config-format)
  (cond
    ((string? a) (format-string a config-format current-indent))
    ( (vector? a)
      (string-append "#"
        (format-sequence a (vector-length a) vector-ref current-indent config config-format)))
    ((pair? a) (any->string-write* a))
    (else (any->string-write* a))))

(define (scm-format-list->string a nesting-depth config)
  (let
    ( (config-format (ht-ref config (q format)))
      (descend-prefix->format-f
        (or (ht-ref config (q descend-prefix->format-f)) descend-prefix->format-f)))
    (tree-transform* a
      (l (expr recurse current-indent)
        (let (format-f (ht-ref descend-prefix->format-f (first expr)))
          (if format-f
            (apply
              (l (r continue?)
                (list r continue? (if continue? (+ 1 current-indent) current-indent)))
              (format-f expr recurse config-format (+ 1 current-indent)))
            (list #f #t (+ 1 current-indent)))))
      (l (expr current-indent)
        (list
          ( (ht-ref ascend-prefix->format-f (first expr) default-format-ascend) expr config-format
            current-indent)
          (- current-indent 1)))
      (l (expr current-indent)
        (list (format-non-list expr current-indent config config-format) current-indent))
      nesting-depth)))

(define (read-hashbang port)
  (let loop ((char (get-char port)) (result null))
    (if (and (eq? #\! char) (eq? #\# (lookahead-char port)))
      (list->string (reverse (pairs (get-char port) char result)))
      (loop (get-char port) (pair char result)))))

(define (extract-hashbang port)
  (if (eq? #\# (lookahead-char port))
    (let (portion (get-string-n port 2))
      (if (string-equal? "#!" portion) (string-append "#!" (read-hashbang port) "\n\n")
        (begin (unget-string port portion) "")))
    ""))

(define (pre-process-line-comments port)
  "replace comments with scheme expressions that the scheme reader will parse"
  (open-input-string
    (string-join
      (port-lines-map
        (l (line)
          (let (space-index (string-skip line #\space))
            (if (and space-index (eq? #\; (string-ref line space-index)))
              (let (content-index (string-skip line #\; space-index))
                (if content-index
                  (any->string-write*
                    (list (q _parsed-semicolon-comment)
                      (string-trim (substring line content-index)) (- content-index space-index)))
                  line))
              line)))
        port)
      "\n")))

(define (primitive-scm-format a current-indent config)
  "(any ...):expressions integer:current-indent r6rs-hashtable:config -> string"
  (apply
    (l (r is-library)
      (string-join-with-vertical-spacing r ""
        (ht-tree-ref config (q format) (q toplevel-vertical-spacing))
        (ht-tree-ref config (q format) (q toplevel-vertical-spacing-oneline))))
    (list
      (map (l (e) (first (scm-format-list->string e current-indent config)))
        (scm-format-transform-tree a (ht-ref-q config transform)))
      (any is-library? a))))

(define* (scm-format a #:optional (current-indent 0) config) "any:scheme-data -> string"
  (primitive-scm-format (list a) 0
    (if config (config-add-defaults config) scm-format-default-config)))

(define* (scm-format-port a #:optional config) "port [r6rs-hashtable] -> string"
  (let*
    ( (config (if config (config-add-defaults config) scm-format-default-config))
      (scm-read (or (ht-ref config (q scm-read)) read)) (hashbang (extract-hashbang a))
      (a (pre-process-line-comments a)))
    (string-append hashbang
      (primitive-scm-format (stream->list (port->stream a scm-read)) 0 config))))

(export ascend-prefix->format-f default-format-ascend
  descend-prefix->format-f scm-format
  scm-format-default-config scm-format-list->string scm-format-port sph-lang-scm-format-description)
