(library (sph lang sc)
  (export
    sc->c
    sc-default-load-paths
    sph-lang-sc-description)
  (import
    (ice-9 match)
    (rnrs exceptions)
    (sph)
    (sph conditional)
    (sph filesystem)
    (sph lang c expressions)
    (sph lang sc expressions)
    (sph lang scheme)
    (sph list)
    (sph string)
    (sph tree)
    (only (guile)
      string-contains
      string-join
      string-split
      string-trim
      string-trim-right
      getenv
      member
      compose))

  (define sph-lang-sc-description
    "an s-expression to c compiler.
     main algorithm: source code is parsed using scheme read, resulting in a list of expressions / syntax tree.
       the syntax tree is traversed top to bottom and eventually bottom to top and matching elements are
       mapped to strings which are joined to the result in the end")

  (define-syntax-rule (add-begin-if-multiple a) (if (length-one? a) (first a) (pair (q begin) a)))
  (define (contains-set? a) "list -> boolean" (tree-contains? a (q set)))

  (define sc-default-load-paths
    (map ensure-trailing-slash
      (if-pass (getenv "SC_LOAD_PATH") (l (a) (string-split a #\:)) (list))))

  (define-as identical-infix-token list "+" "-" "<" ">" "<=" ">=" "*" "/")

  (define-as translated-infix-token list
    "equal_p" "or"
    "and" "=" "bit_or" "bit_and" "bit_xor" "modulo" "bit_shift_right" "bit_shift_left" "eqv_p" "eq_p")

  (define (translate-infix-token a)
    (string-case a (("=" "equal_p" "eqv_p" "eq_p") "==")
      ("and" "&&") ("bit_or" "|")
      ("bit_and" "&") ("or" "||")
      ("modulo" "%") ("bit_shift_right" ">>")
      ("bit_shift_left" "<<") ("bit_xor" "^") (else (raise (q fail-translate-infix-token)))))

  (define (ascend-expr->c a)
    "any -> string
     handles expressions that are processed when ascending the tree"
    (string-case (first a) ("not" (string-append "!" (apply string-append (tail a))))
      ("pointer_get"
        (let (a (tail a)) (c-pointer-deref (first a) (if (null? (tail a)) #f (first (tail a))))))
      ("address_of" (apply c-address-of (tail a))) ("convert_type" (apply c-convert-type (tail a)))
      ("begin" (sc-join-expressions (tail a))) ("struct_get" (apply c-struct-get (tail a)))
      ("return" (if (null? (tail a)) "return" (sc-apply (first a) (tail a))))
      ("goto" (string-join a " "))
      ("label" (string-append (first (tail a)) ":" (sc-join-expressions (tail (tail a)))))
      ("signed" (string-trim-right (first (tail a)) #\u))
      (identical-infix-token (parenthesise (string-join (tail a) (first a))))
      (translated-infix-token
        (parenthesise
          (string-append
            (string-join
              ;the map is for cases like a&&b=c where a lvalue error would occur for b=c
              (map (l (a) (if (string-contains a "=") (parenthesise a) a)) (tail a))
              (translate-infix-token (first a))))))
      ("bit_not" (string-append "~" (first (tail a))))
      ("pre_stringify" (c-stringify (first (tail a))))
      ("pre_string_concat" (string-join (tail a) " "))
      ("compound_statement" (c-compound (sc-join-expressions (tail a))))
      (else (if (list? a) (sc-apply (first a) (tail a)) a))))

  (define (descend-expr->sc a compile load-paths)
    "list procedure list -> list
     handles expressions that are processed when descending the tree. the result is parsed again"
    (case (first a)
      ( (array-get)
        (match (tail a) ((a index) (list (q pointer-get) a index))
          ( (a index ... index-last)
            (list (q pointer-get) a (list (q +) (pair (q *) index) index-last)))))
      ( (array-set)
        (let (array (first (tail a)))
          (pair (q begin)
            (map-with-index (l (index value) (list (q set) (list (q array-get) array index) value))
              (tail (tail a))))))
      ( (array-set-index)
        (let (array (first (tail a)))
          (pair (q begin)
            (map-slice 2 (l (index value) (list (q set) (list (q array-get) array index) value))
              (tail (tail a))))))
      ((case case*) (apply sc-case (if (equal? (q case*) (first a)) (q cond*) (q cond)) (tail a)))
      ( (cond*)
        (let (conditions (reverse (tail a)))
          (fold
            (l (condition alternate)
              (list (q if*) (first condition) (add-begin-if-multiple (tail condition)) alternate))
            (match (first conditions) (((quote else) body ...) (add-begin-if-multiple body))
              ((test consequent ...) (list (q if*) test (add-begin-if-multiple consequent))))
            (tail conditions))))
      ( (pointer-set)
        (match (tail a)
          ((pointer value) (qq (set (pointer-get (unquote pointer)) (unquote value))))))
      ( (pre-define-if-not-defined)
        (pair (q begin)
          (map-slice 2
            (l (name value)
              (let
                (identifier (match name (((? not-preprocessor-keyword? name) _ ...) name) (_ name)))
                (qq
                  (pre-if-not-defined (unquote identifier)
                    (pre-define (unquote name) (unquote value))))))
            (tail a))))
      ((sc-include) (sc-include-sc load-paths (tail a)))
      ((sc-include-once) (sc-include-sc-once load-paths (tail a)))
      ( (: struct-pointer-get)
        (match (tail a)
          ( (identifier fields ...)
            (qq (struct-get (pointer-get (unquote identifier)) (unquote-splicing fields))))))
      ( (struct-pointer-set)
        (match (tail a)
          ( (identifier field/value ...)
            (qq (struct-set (pointer-get (unquote identifier)) (unquote-splicing field/value))))))
      ( (struct-set)
        (let (struct (first (tail a)))
          (pair (q begin)
            (map-slice 2 (l (field value) (list (q set) (list (q struct-get) struct field) value))
              (tail (tail a))))))
      (else #f)))

  (define (c-for init test update body)
    (string-append "for(" init ";" test ";" update "){" body "}"))

  (define (descend-expr->c a compile)
    "list procedure -> string
     handles expressions that are processed when descending the tree. the result is not parsed again.
     this is for expressions that create syntax that can not be created with other sc syntax"
    (case (first a)
      ((array-literal) (c-compound (map compile (tail a))))
      ((cond) (sc-cond (tail a) compile))
      ((declare) (sc-declare (tail a) compile))
      ((define) (sc-define (tail a) compile))
      ( (do-while)
        (match (tail a)
          ( (test body ...)
            (string-append "do" (c-compound (compile (pair (q begin) body)))
              "while" (parenthesise (compile test))))))
      ((enum) (sc-enum (tail a)))
      ( (for)
        (match (tail a)
          ( ( (init test update) body ...)
            (c-for (compile init) (compile test) (compile update) (compile (pair (q begin) body))))))
      ((function-pointer) (apply sc-function-pointer compile "" (tail a)))
      ( (if)
        (apply c-if-statement (compile (first (tail a)))
          (map (l (e) (compile (list (q begin) e))) (tail (tail a)))))
      ( (if*)
        (apply c-if
          (map
            (l (e)
              (match e
                ( ( (quote begin) body ...)
                  (parenthesise
                    (string-join
                      (map
                        (l (e)
                          (if (and (list? e) (contains-set? e)) (parenthesise (compile e))
                            (compile e)))
                        body)
                      ",")))
                (_ (if (and (list? e) (contains-set? e)) (parenthesise (compile e)) (compile e)))))
            (tail a))))
      ( (let*)
        (c-compound
          (match (tail a)
            ( ( ( (names values ...) ...) body ...)
              (compile
                (pair (q begin)
                  (append
                    (map (l (n v) (pairs (if (length-one? v) (q set) (q define)) n v)) names values)
                    body)))))))
      ( (pre-define)
        (if (= 2 (length a)) (cp-pre-define (sc-identifier (second a)) #f #f)
          (string-join
            (map-slice 2
              (l (name value)
                (match name
                  ((name parameter ...) (sc-macro-function name parameter (list value) compile))
                  (_ (cp-pre-define (sc-identifier name) (compile value) #f))))
              (tail a))
            "\n")))
      ((pre-pragma) (string-append "#pragma " (string-join (map sc-identifier (tail a)) " ") "\n"))
      ((struct union) (sc-struct-or-union (first a) (tail a) compile))
      ( (struct-literal)
        (string-append
          (c-compound (map (l (a) (if (list? a) (map compile a) (compile a))) (tail a)))))
      ((pre-undefine) (string-join (map (compose cp-undef sc-identifier) (tail a)) "\n" (q suffix)))
      ( (pre-let)
        ; descend-expr->sc currently would add an uneccessary semicolon at the end
        (match (tail a)
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
      ((pre-include) (sc-pre-include (tail a)))
      ((pre-concat) (cp-concat (map sc-identifier (tail a))))
      ((pre-stringify) (cp-stringify (apply sc-identifier (tail a))))
      ((pre-if) (scp-if (q if) (tail a) compile))
      ((pre-if-defined) (scp-if (q ifdef) (tail a) compile))
      ((pre-if-not-defined) (scp-if (q ifndef) (tail a) compile))
      ((quote) (list-ref a 1))
      ((sc-comment) (string-append "\n/* " (second a) " */\n"))
      ( (set)
        (match (tail a)
          ( (name-1 value-1 name-2 value-2 rest ...)
            (sc-join-expressions (map-slice 2 c-set (map compile (tail a)))))
          ((name value) (c-set (compile name) (compile value)))))
      ( (while)
        (match (tail a)
          ( (test body ...)
            (string-append "while" (parenthesise (compile test))
              (c-compound (compile (pair (q begin) body)))))))
      (else #f)))

  (define (descend-proc load-paths)
    (l (a compile)
      (let (r (descend-expr->sc a compile load-paths))
        (if r (list r #t) (let (r (descend-expr->c a compile)) (if r (list r #f) (list #f #t)))))))

  (define* (sc->c a #:optional (load-paths sc-default-load-paths))
    "expression [(string ...)] -> string"
    (string-trim
      (regexp-replace (tree-transform a (descend-proc load-paths) ascend-expr->c sc-value) "\n\n+"
        "\n"))))
