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
    (only (guile)
      string-contains
      string-join
      string-split
      string-trim
      getenv
      compose)
    (only (sph conditional) if-pass)
    (only (sph filesystem) ensure-trailing-slash)
    (only (sph tree) tree-transform))

  (define sph-lang-sc-description
    "an s-expression to c compiler.
     main algorithm: source code is parsed using scheme read, resulting in a list of expressions / syntax tree.
       the syntax tree is traversed top to bottom and eventually bottom to top and matching elements are
       mapped to strings which are joined to the result in the end")

  (define sc-default-load-paths
    (map ensure-trailing-slash
      (if-pass (getenv "SC_LOAD_PATH") (l (a) (string-split a #\:)) (list))))

  (define (descend-expr->sc prefix a compile load-paths)
    "list procedure list -> list
     handles expressions that are processed when descending the tree. the result is parsed again"
    (case prefix
      ( (array-set)
        (let (array (first a))
          (pair (q begin)
            (map-slice 2 (l (index value) (list (q set) (list (q array-get) array index) value))
              (tail a)))))
      ((case case*) (sc-case (equal? (q case*) prefix) a compile))
      ( (cond*)
        (let (conditions (reverse a))
          (fold
            (l (condition alternate)
              (list (q if*) (first condition) (add-begin-if-multiple (tail condition)) alternate))
            (match (first conditions) (((quote else) body ...) (add-begin-if-multiple body))
              ((test consequent ...) (list (q if*) test (add-begin-if-multiple consequent))))
            (tail conditions))))
      ( (pre-define-if-not-defined)
        (pair (q begin)
          (map-slice 2
            (l (name value)
              (let
                (identifier (match name (((? not-preprocessor-keyword? name) _ ...) name) (_ name)))
                (qq
                  (pre-if-not-defined (unquote identifier)
                    (pre-define (unquote name) (unquote value))))))
            a)))
      ((sc-include) (sc-include-sc load-paths a))
      ((sc-include-once) (sc-include-sc-once load-paths a))
      ( (struct-set)
        (let (struct (first a))
          (pair (q begin)
            (map-slice 2 (l (field value) (list (q set) (list (q struct-get) struct field) value))
              (tail a)))))
      (else #f)))

  (define (descend-expr->c prefix a compile)
    "list procedure -> string
     handles expressions that are processed when descending the tree. the result is not parsed again.
     this is for expressions that create syntax that can not be created with other sc syntax"
    (case prefix
      ((+ - * /) (parenthesise (string-join (map compile a) (symbol->string prefix))))
      ((= < > <= >=) (sc-numeric-boolean prefix a compile))
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
      ( (do-while)
        (match a
          ( (test body ...)
            (string-append "do" (c-compound (compile (pair (q begin) body)))
              "while" (parenthesise (compile test))))))
      ((enum) (sc-enum a))
      ((for) (sc-for a compile))
      ((function-pointer) (apply sc-function-pointer compile "" a))
      ((goto) (string-append "goto " (compile (first a))))
      ((if) (sc-if a compile))
      ((if*) (sc-if* a compile))
      ((label) (string-append (compile (first a)) ":" (sc-join-expressions (map compile (tail a)))))
      ( (let*)
        (c-compound
          (match a
            ( ( ( (names values ...) ...) body ...)
              (compile
                (pair (q begin)
                  (append
                    (map (l (n v) (pairs (if (length-one? v) (q set) (q define)) n v)) names values)
                    body)))))))
      ((not) (c-not (compile (first a))))
      ((pointer-get) (apply c-pointer-get (map compile a)))
      ((pre-define) (sc-pre-define a compile))
      ((pre-pragma) (string-append "#pragma " (string-join (map sc-identifier a) " ") "\n"))
      ((pre-undefine) (string-join (map (compose cp-undef sc-identifier) a) "\n" (q suffix)))
      ( (pre-let)
        ; descend-expr->sc currently would add an uneccessary semicolon at the end
        (match a
          ( ( (names+values ...) body ...)
            (string-append
              (string-join (map-slice 2 (l (n v) (compile (list (q pre-define) n v))) names+values)
                "\n" (q suffix))
              (compile (pair (q begin) body))
              (string-join
                (map-slice 2 (l (n v) (compile (list (q pre-undefine) (if (pair? n) (first n) n))))
                  names+values)
                "\n" (q prefix))))
          (_ (raise (q syntax-error-for-pre-let)))))
      ((pre-include) (sc-pre-include a))
      ((pre-concat) (cp-concat (map sc-identifier a)))
      ((pre-if) (sc-pre-if (q if) a compile))
      ((pre-if-defined) (sc-pre-if (q ifdef) a compile))
      ((pre-if-not-defined) (sc-pre-if (q ifndef) a compile))
      ((pre-stringify) (cp-stringify (compile (first a))))
      ((pre-string-concat) (string-join (map compile a) " "))
      ((return) (if (null? a) "return" (sc-apply "return" (map compile a))))
      ((sc-insert) (first a))
      ((sc-comment) (string-append "\n/* " (string-join a " ") " */\n"))
      ((set) (sc-set a compile))
      ((struct union) (sc-struct-or-union prefix a compile))
      ((struct-get) (apply c-struct-get (map compile a)))
      ( (struct-literal)
        (string-append (c-compound (map (l (a) (if (list? a) (map compile a) (compile a))) a))))
      ((: struct-pointer-get) (apply c-struct-pointer-get (map compile a)))
      ((while) (sc-while a compile))
      (else (sc-apply (compile prefix) (map compile a)))))

  (define (descend-proc load-paths)
    (l (a compile)
      (let* ((prefix (first a)) (a (tail a)) (b (descend-expr->sc prefix a compile load-paths)))
        (if b (list b #t)
          (let (b (descend-expr->c prefix a compile)) (if b (list b #f) (list #f #t)))))))

  (define* (sc->c a #:optional (load-paths sc-default-load-paths))
    "expression [(string ...)] -> string"
    (and (sc-syntax-check (list a) load-paths)
      (string-trim
        (regexp-replace (tree-transform a (descend-proc load-paths) identity sc-value) "\n\n+" "\n")))))
