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
    (sph alist)
    (sph conditional)
    (sph filesystem)
    (sph lang c expressions)
    (sph lang sc check)
    (sph lang sc expressions)
    (sph lang scheme)
    (sph list)
    (sph string)
    (only (guile)
      string-contains
      string-join
      string-split
      string-trim
      string-trim-right
      getenv
      member
      compose)
    (only (sph tree) tree-every tree-transform))

  (define sph-lang-sc-description
    "an s-expression to c compiler.
     main algorithm: source code is parsed using scheme read, resulting in a list of expressions / syntax tree.
       the syntax tree is traversed top to bottom and eventually bottom to top and matching elements are
       mapped to strings which are joined to the result in the end")

  (define-syntax-rule (add-begin-if-multiple a) (if (length-one? a) (first a) (pair (q begin) a)))

  (define sc-default-load-paths
    (map ensure-trailing-slash
      (if-pass (getenv "SC_LOAD_PATH") (l (a) (string-split a #\:)) (list))))

  (define (prefix->infix-string prefix arguments) "string string ... -> string"
    (parenthesise (string-join arguments prefix)))

  (define (ascend-expr->c a)
    "any -> string
     handles expressions that are processed when ascending the tree.
     arguments have already been compiled"
    (if (list? a) (sc-apply (first a) (tail a)) a))

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
      ((+ - * /) (prefix->infix-string (symbol->string prefix) (map compile a)))
      ( (= < > <= >=)
        (let (operator (if (eq? (q =) prefix) "==" (symbol->string prefix)))
          (string-join
            (map-segments 2 (l (a b) (string-append "(" a operator b ")")) (map compile a)) "&&")))
      ( (and bit-and bit-or bit-shift-right bit-shift-left bit-xor modulo or)
        (prefix->infix-string
          (case prefix
            ((and) "&&")
            ((bit-or) "|")
            ((bit-and) "&")
            ((or) "||")
            ((modulo) "%")
            ((bit-shift-right) ">>")
            ((bit-shift-left) "<<")
            ((bit-xor) "^"))
          (map
            (compose
              (l (a) "map cases like a&&b=c where a lvalue error would occur for b=c"
                (if (string-contains a "=") (parenthesise a) a))
              compile)
            a)))
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
      ( (for)
        (let
          (comma-join
            (l (a)
              (match a (((quote begin) a ...) (string-join (map compile a) ","))
                (((? symbol?) _ ...) (compile a)) (_ (string-join (map compile a) ",")))))
          (match a
            ( ( (init test update) body ...)
              (c-for (comma-join init) (compile test)
                (comma-join update) (compile (pair (q begin) body)))))))
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
      ( (pre-define)
        (if (= 1 (length a)) (cp-pre-define (sc-identifier (first a)) #f #f)
          (string-join
            (map-slice 2
              (l (name value)
                (match name
                  ((name parameter ...) (sc-macro-function name parameter (list value) compile))
                  (_ (cp-pre-define (sc-identifier name) (compile value) #f))))
              a)
            "\n")))
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
      ((pre-if) (scp-if (q if) a compile))
      ((pre-if-defined) (scp-if (q ifdef) a compile))
      ((pre-if-not-defined) (scp-if (q ifndef) a compile))
      ((pre-stringify) (cp-stringify (compile (first a))))
      ((pre-string-concat) (string-join (map compile a) " "))
      ((return) (if (null? a) "return" (sc-apply "return" (map compile a))))
      ((sc-insert) (first a))
      ((sc-comment) (string-append "\n/* " (first a) " */\n"))
      ((set) (sc-set a compile))
      ((struct union) (sc-struct-or-union prefix a compile))
      ((struct-get) (apply c-struct-get (map compile a)))
      ( (struct-literal)
        (string-append (c-compound (map (l (a) (if (list? a) (map compile a) (compile a))) a))))
      ((: struct-pointer-get) (apply c-struct-pointer-get (map compile a)))
      ( (while)
        (match a
          ( (test body ...)
            (string-append "while" (parenthesise (compile test))
              (c-compound (compile (pair (q begin) body)))))))
      (else #f)))

  (define (descend-proc load-paths)
    (l (a compile)
      (let ((prefix (first a)) (a (tail a)))
        (let (b (descend-expr->sc prefix a compile load-paths))
          (if b (list b #t)
            (let (b (descend-expr->c prefix a compile)) (if b (list b #f) (list #f #t))))))))

  (define* (sc->c a #:optional (load-paths sc-default-load-paths))
    "expression [(string ...)] -> string"
    (and (sc-syntax-check (list a) load-paths)
      (string-trim
        (regexp-replace (tree-transform a (descend-proc load-paths) ascend-expr->c sc-value)
          "\n\n+" "\n")))))
