(library (sph lang sc)
  (export
    sc->c
    sc-default-load-paths
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

  (define-as syntax-examples alist-q
    ; these are used in error messages as examples of what is allowed
    define
    (list-q (name type value) ((name parameters ...) (return-type ...) body ...)
      ((name parameters ...) return-type body ...))
    set (list-q (variable value) (variable value variable value variable/value ...)))

  (define (syntax-examples-get name) "prepend the prefix symbol to example argument patterns"
    (and-let* ((examples (alist-ref syntax-examples name)))
      (map (l (a) (if (list? a) (pair name a) a)) examples)))

  (define (sc-syntax-error? a) (and (list? a) (not (null? a)) (eq? (q sc-syntax-error) (first a))))

  (define* (sc-syntax-error #:optional irritant syntax-name expected)
    "false/any false/symbol false/(any ...) | exception"
    (raise
      (pair (q sc-syntax-error)
        (compact
          (list (and irritant (pair (q irritant) irritant))
            (or (and expected (pair (q expected) expected))
              (and syntax-name
                (and-let* ((examples (syntax-examples-get syntax-name)))
                  (pair (q expected) examples)))))))))

  (define-syntax-rule (add-begin-if-multiple a) (if (length-one? a) (first a) (pair (q begin) a)))
  (define (contains-set? a) "list -> boolean" (tree-contains? a (q set)))

  (define sc-default-load-paths
    (map ensure-trailing-slash
      (if-pass (getenv "SC_LOAD_PATH") (l (a) (string-split a #\:)) (list))))

  (define (translate-infix-token a)
    (string-case a ("and" "&&")
      ("bit_or" "|") ("bit_and" "&")
      ("or" "||") ("modulo" "%")
      ("bit_shift_right" ">>") ("bit_shift_left" "<<")
      ("bit_xor" "^") (else (raise (q fail-translate-infix-token)))))

  (define (prefix->infix-string prefix arguments) "string string ... -> string"
    (parenthesise (string-join arguments prefix)))

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
      ( ("=" "<" ">" "<=" ">=")
        (let* ((prefix (first a)) (prefix (if (string-equal? "=" prefix) "==" prefix)))
          (string-join (map-segments 2 (l (a b) (string-append "(" a prefix b ")")) (tail a)) "&&")))
      (("+" "-" "*" "/") (prefix->infix-string (first a) (tail a)))
      ( ("or" "and" "bit_or" "bit_and" "bit_xor" "modulo" "bit_shift_right" "bit_shift_left")
        (prefix->infix-string (translate-infix-token (first a))
          (map
            (l (a) "map cases like a&&b=c where a lvalue error would occur for b=c"
              (if (string-contains a "=") (parenthesise a) a))
            (tail a))))
      ("bit_not" (string-append "~" (second a))) ("pre_stringify" (c-stringify (second a)))
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

  (define (descend-expr->c a compile)
    "list procedure -> string
     handles expressions that are processed when descending the tree. the result is not parsed again.
     this is for expressions that create syntax that can not be created with other sc syntax"
    (case (first a)
      ((array-literal) (c-compound (map compile (tail a))))
      ((cond) (sc-cond (tail a) compile))
      ((declare) (or (sc-declare (tail a) compile) (sc-syntax-error a (q declare))))
      ((define) (or (sc-define (tail a) compile) (sc-syntax-error a (q define))))
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
      ((set) (or (sc-set (tail a) compile) (sc-syntax-error a (q set))))
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
