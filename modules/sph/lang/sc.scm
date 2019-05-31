(library (sph lang sc)
  (export
    sc->c
    sc-default-load-paths
    sc-syntax-check
    sc-syntax-error
    sc-syntax-error?
    sph-lang-sc-description)
  (import
    (ice-9 match)
    (rnrs exceptions)
    (sph)
    (sph lang c expressions)
    (sph lang sc check)
    (sph lang sc expressions)
    (sph list)
    (sph string)
    (only (guile) string-contains string-join string-split string-trim getenv compose)
    (only (sph filesystem) ensure-trailing-slash)
    (only (sph tree) tree-transform))

  (define sph-lang-sc-description
    "an s-expression to c compiler.
     main algorithm: source code is parsed using scheme read, resulting in a list of expressions / syntax tree.
       the syntax tree is traversed top to bottom and eventually bottom to top and matching elements are
       mapped to strings which are joined to the result in the end")

  (define (sc-default-load-paths)
    (map ensure-trailing-slash (let (a (getenv "SC_LOAD_PATH")) (if a (string-split a #\:) null))))

  (define (descend-expr->sc prefix a compile state)
    "list procedure list -> list
     handles expressions that are processed when descending the tree. the result is parsed again"
    (case prefix
      ( (array-set)
        (let (array (first a))
          (pair (q begin)
            (map-slice 2 (l (index value) (list (q set) (list (q array-get) array index) value))
              (tail a)))))
      ((case case*) (sc-case (eq? (q case*) prefix) a compile (l (a) (sc->sc a state))))
      ((cond*) (sc-cond* a compile))
      ((pre-define-if-not-defined) (sc-pre-define-if-not-defined a compile))
      ((pre-define) (sc-pre-define-multiple a compile))
      ((sc-include) (sc-include-sc state a (l (a) (sc->sc a state))))
      ((sc-include-once) (sc-include-sc-once state a (l (a) (sc->sc a state))))
      ((set) (sc-set-multiple a compile))
      ( (struct-set)
        (let (struct (first a))
          (pair (q begin)
            (map-slice 2 (l (field value) (list (q set) (list (q struct-get) struct field) value))
              (tail a)))))
      (else #f)))

  (define (descend-expr->c prefix a compile state)
    "list procedure -> string
     handles expressions that are processed when descending the tree. the result is not parsed again.
     this is for expressions that create syntax that can not be created with other sc syntax"
    (case prefix
      ((+ - * /) (parenthesise (string-join (map compile a) (symbol->string prefix))))
      ((!= = < > <= >=) (sc-numeric-boolean prefix a compile))
      ( (and bit-and bit-or bit-shift-right bit-shift-left bit-xor modulo or)
        (parenthesise
          (string-join
            (map
              (l (a) "consider cases like a&&b=c where a lvalue error would occur for b=c"
                (if (contains-set? a) (parenthesise (compile a)) (compile a)))
              a)
            (case prefix
              ((and) "&&")
              ((bit-or) "|")
              ((bit-and) "&")
              ((or) "||")
              ((modulo) "%")
              ((bit-shift-right) ">>")
              ((bit-shift-left) "<<")
              ((bit-xor) "^")))))
      ((address-of) (c-address-of (apply string-append (map compile a))))
      ((array-get) (apply c-array-get (map compile a)))
      ((array-literal) (c-compound (map compile a)))
      ((begin) (sc-join-expressions (map compile a)))
      ((bit-not) (c-bit-not (compile (first a))))
      ((compound-statement) (c-compound (sc-join-expressions (map compile a))))
      ((cond) (sc-cond a compile))
      ((convert-type) (apply c-convert-type (map compile a)))
      ((declare) (sc-declare a compile))
      ((define) (sc-define a compile))
      ((do-while) (sc-do-while a compile))
      ((enum) (sc-enum a))
      ((for) (sc-for a compile))
      ((function-pointer) (apply sc-function-pointer compile "" a))
      ((goto) (string-append "goto " (compile (first a))))
      ((if) (sc-if a compile))
      ((if*) (sc-if* a compile))
      ((label) (string-append (compile (first a)) ":" (sc-join-expressions (map compile (tail a)))))
      ((let*) (sc-let* a compile))
      ((not) (c-not (compile (first a))))
      ((pointer-get) (apply c-pointer-get (map compile a)))
      ((pre-define) (sc-pre-define state a compile))
      ((pre-pragma) (string-append "#pragma " (string-join (map sc-identifier a) " ") "\n"))
      ((pre-undefine) (string-join (map (compose cp-undef sc-identifier) a) "\n" (q suffix)))
      ((pre-let*) (sc-pre-let* a compile))
      ((pre-include) (sc-pre-include a))
      ((pre-concat) (cp-concat (map sc-identifier a)))
      ((pre-cond) (sc-pre-cond (q if) a compile))
      ((pre-cond-defined) (sc-pre-cond (q ifdef) a compile))
      ((pre-cond-not-defined) (sc-pre-cond (q ifndef) a compile))
      ((pre-if) (sc-pre-if (q if) a compile))
      ((pre-if-defined) (sc-pre-if (q ifdef) a compile))
      ((pre-if-not-defined) (sc-pre-if (q ifndef) a compile))
      ((pre-stringify) (cp-stringify (compile (first a))))
      ((pre-string-concat) (string-join (map compile a) " "))
      ((return) (if (null? a) "return" (sc-apply state (q return) a compile)))
      ((sc-insert) (first a))
      ((sc-comment) (string-append "/* " (string-join a "\n") " */\n"))
      ((sc-no-semicolon) (sc-no-semicolon a compile))
      ((set) (sc-set a compile))
      ((set+) (sc-set a compile "+="))
      ((set-) (sc-set a compile "-="))
      ((set*) (sc-set a compile "*="))
      ((set/) (sc-set a compile "/="))
      ((struct union) (sc-struct-or-union prefix a compile))
      ((struct-get) (apply c-struct-get (map compile a)))
      ((struct-literal) (sc-struct-literal a compile))
      ((: struct-pointer-get) (apply c-struct-pointer-get (map compile a)))
      ((while) (sc-while a compile))
      (else (sc-apply state prefix a compile))))

  (define (sc->sc a state) "expand only sc->sc expressions"
    (tree-transform a
      (l (a compile)
        (let* ((prefix (first a)) (a (tail a)) (b (descend-expr->sc prefix a compile state)))
          (if b (list b #f) (list #f #t))))
      identity identity))

  (define* (sc->c a #:optional load-paths state)
    "expression [(string ...) sc-state] -> string
    load-paths is only used if state is not given or false"
    (let (state (or state (sc-state-new (or load-paths (sc-default-load-paths)))))
      (and (sc-syntax-check (list a) state)
        (string-trim
          (regexp-replace
            (tree-transform (sc->sc a state)
              (l (a compile)
                (let*
                  ((prefix (first a)) (a (tail a)) (b (descend-expr->c prefix a compile state)))
                  (if b (list b #f) (list #f #t))))
              identity sc-value)
            "\n\n+" "\n"))))))
