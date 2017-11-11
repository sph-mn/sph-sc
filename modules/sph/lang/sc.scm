(library (sph lang sc)
  (export
    sc->c
    sc-default-load-paths
    sph-lang-sc-description)
  (import
    (guile)
    (ice-9 match)
    (sph base)
    (sph conditional)
    (sph lang c expressions)
    (sph lang sc expressions)
    (sph lang scheme)
    (sph tree))

  (define sph-lang-sc-description "a scheme data to c compiler")

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

  (define-syntax-rule (add-begin-if-multiple a) (if (length-one? a) (first a) (pair (q begin) a)))
  (define sc-included-paths (ht-create-string))

  (define (sc-path->full-path load-paths path)
    (let* ((path (string-append path ".sc")) (path-found (search-load-path path load-paths)))
      (if path-found path-found
        (raise
          (list (q file-not-accessible)
            (string-append (any->string path) " not found in " (any->string load-paths)))))))

  (define (sc-include-sc-once load-paths name/path)
    "(string ...) (symbol/string ...) -> list
     pre-include-once should be preferred for modules that are available to other code because
     sc can not prevent repeated inclusion with c files that were built using sc-include-sc"
    (pair (q begin)
      (map-slice 2
        (l (name path)
          (let
            ( (path (sc-path->full-path load-paths path))
              (variable-name (sc-pre-include-variable name)))
            (if (ht-ref sc-included-paths path) (q (begin))
              (begin (ht-set! sc-included-paths path #t)
                (list (q pre-if-not-defined) variable-name
                  (pairs (q begin) (sc-pre-include-define name) (file->datums path)))))))
        name/path)))

  (define (sc-include-sc load-paths paths) "(string ...) (string ...) -> list"
    (pair (q begin)
      (append-map (l (a) (let (a (sc-path->full-path load-paths a)) (file->datums a read))) paths)))

  (define (struct-or-union-body elements compile)
    (string-join
      (map
        (l (e)
          (match e
            ( (name type (? integer? bits))
              (string-append
                (string-join (map (l (a) (sc-compile-type a compile)) type) " " (q suffix))
                (sc-identifier name) ":" (sc-value bits)))
            ( (name type)
              (if (sc-function-pointer? type)
                (apply sc-function-pointer compile (compile name) (tail type))
                (string-append (sc-compile-type type compile) " " (compile name))))))
        elements)
      ";" (q suffix)))

  (define (contains-set? a) "list -> boolean" (tree-contains? a (q set)))

  (define (ascend-expr->c a)
    "any -> string
     handles expressions that are processed when ascending the tree"
    (string-case (first a) ("not" (string-append "!" (apply string-append (tail a))))
      ("deref"
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
      ( (struct-set)
        (let (struct (first (tail a)))
          (pair (q begin)
            (map-slice 2 (l (field value) (list (q set) (list (q struct-get) struct field) value))
              (tail (tail a))))))
      ( (struct-pointer-get)
        (match (tail a)
          ( (identifier fields ...)
            (qq (struct-get (deref (unquote identifier)) (unquote-splicing fields))))))
      ( (struct-pointer-set)
        (match (tail a)
          ( (identifier field/value ...)
            (qq (struct-set (deref (unquote identifier)) (unquote-splicing field/value))))))
      ( (array-set-index)
        (let (array (first (tail a)))
          (pair (q begin)
            (map-slice 2 (l (index value) (list (q set) (list (q array-get) array index) value))
              (tail (tail a))))))
      ( (array-set)
        (let (array (first (tail a)))
          (pair (q begin)
            (map-with-index (l (index value) (list (q set) (list (q array-get) array index) value))
              (tail (tail a))))))
      ( (array-get)
        (match (tail a) ((a index) (list (q deref) a index))
          ((a index ... index-last) (list (q deref) a (list (q +) (pair (q *) index) index-last)))))
      ((sc-include) (sc-include-sc load-paths (tail a)))
      ((sc-include-once) (sc-include-sc-once load-paths (tail a)))
      ( (cond cond*)
        (let ((cond (reverse (tail a))) (symbol-if (if (eqv? (first a) (q cond*)) (q if*) (q if))))
          (fold
            (l (cond alternate)
              (list symbol-if (first cond) (add-begin-if-multiple (tail cond)) alternate))
            (match (first cond) (((quote else) body ...) (add-begin-if-multiple body))
              ((test consequent ...) (list symbol-if test (add-begin-if-multiple consequent))))
            (tail cond))))
      ((case case*) (apply sc-case (if (equal? (q case*) (first a)) (q cond*) (q cond)) (tail a)))
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
      (else #f)))

  (define (descend-expr->c a compile)
    "list procedure -> string
     handles expressions that are processed when descending the tree. the result is not parsed again.
     this is for expressions that create syntax that can not be created with other sc syntax"
    (case (first a)
      ( (set)
        (match (tail a)
          ( (name-1 value-1 name-2 value-2 rest ...)
            (sc-join-expressions (map-slice 2 c-set (map compile (tail a)))))
          ((name value) (c-set (compile name) (compile value)))))
      ( (define)
        (match (tail a)
          ( ( ( (? not-preprocessor-keyword? name) parameter ...)
              ((? not-function-pointer-symbol? return-type) types ...) body ...)
            (sc-function compile name return-type body parameter types))
          ( ( ( (? not-preprocessor-keyword? name)) return-type body ...)
            (sc-function compile name return-type body (list) (list)))
          ( (name-1 type-1 name-2 type-2 rest ...)
            (sc-join-expressions (map-slice 2 (l a (apply sc-define compile a)) (tail a))))
          ((name type value ...) (sc-define compile name type (if (null? value) #f (first value))))))
      ( (if)
        (apply c-if-statement (compile (first (tail a)))
          (map (l (e) (compile (list (q begin) e))) (tail (tail a)))))
      ( (if-expression if*)
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
      ( (pre-define)
        (match (tail a)
          (((name parameter ...) body ...) (sc-macro-function name parameter body compile))
          ( (name-1 value-1 name-2 value-2 rest ...)
            (string-join
              (map-slice 2 (l (name value) (cp-pre-define (sc-identifier name) (compile value) #f))
                (tail a))
              "\n"))
          ( (name body ...)
            (cp-pre-define (sc-identifier name)
              (string-trim-right (sc-join-expressions (map compile body) "\\\n  ") #\;) #f))))
      ( (struct union)
        (let (tail-a (tail a))
          (apply
            (l (name body)
              (c-statement (string-append (symbol->string (first a)) name)
                (struct-or-union-body body compile)))
            (if (symbol? (first tail-a))
              (list (string-append " " (sc-identifier (first tail-a))) (tail tail-a))
              (list "" tail-a)))))
      ((define-type) (apply sc-define-type compile (tail a)))
      ( (define-array)
        (match (tail a)
          ( (name type (size ...) values ...)
            (c-define-array (compile name) (compile type)
              (if (null? size) (list "") (map compile size))
              (if (null? values) #f (map compile values))))))
      ((array-literal) (c-compound (map compile (tail a))))
      ( (struct-literal)
        (string-append
          (c-compound (map (l (a) (if (list? a) (map compile a) (compile a))) (tail a)))))
      ((enum) (sc-enum (tail a)))
      ( (while)
        (match (tail a)
          ( (test body ...)
            (string-append "while" (parenthesise (compile test))
              (c-compound (compile (pair (q begin) body)))))))
      ( (do-while)
        (match (tail a)
          ( (test body ...)
            (string-append "do" (c-compound (compile (pair (q begin) body)))
              "while" (parenthesise (compile test))))))
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
      ((pre-include) (sc-pre-include (tail a))) ((pre-include-once) (sc-pre-include-once (tail a)))
      ((function-pointer) (apply sc-function-pointer compile "" (tail a)))
      ((pre-concat) (cp-concat (map sc-identifier (tail a))))
      ((pre-stringify) (cp-stringify (apply sc-identifier (tail a))))
      ((pre-if) (scp-if (q if) (tail a) compile))
      ((pre-if-defined) (scp-if (q ifdef) (tail a) compile))
      ((pre-if-not-defined) (scp-if (q ifndef) (tail a) compile)) ((quote) (list-ref a 1))
      ( (let*)
        (c-compound
          (match (tail a)
            ( ( ( (names values ...) ...) body ...)
              (compile
                (pair (q begin)
                  (append
                    (map (l (n v) (pairs (if (length-one? v) (q set) (q define)) n v)) names values)
                    body)))))))
      ((sc-comment) (string-append "\n/* " (second a) " */")) (else #f)))

  (define (descend-proc load-paths)
    (l (a compile)
      (let (r (descend-expr->sc a compile load-paths))
        (if r (list r #t) (let (r (descend-expr->c a compile)) (if r (list r #f) (list #f #t)))))))

  (define* (sc->c a #:optional (load-paths sc-default-load-paths))
    "expression [(string ...)] -> string"
    (string-replace-string (tree-transform a (descend-proc load-paths) ascend-expr->c sc-value)
      "\n\n" "\n")))
