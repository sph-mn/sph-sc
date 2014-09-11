;a scheme-datum to c compiler
(library (sph lang sc)
  (export sc->c)
  (import
    (rnrs base)
    (except (srfi srfi-1) map)
    (guile)
    (ice-9 match)
    (sph)
    (sph conditional)
    (only (sph string) string-replace-string string-case parenthesise)
    (sph lang c expressions)
    (sph lang sc expressions)
    (only (sph filesystem) search-load-path ensure-trailing-slash)
    (only (sph read-write) file->datums)
    (only (sph list) map-slice length-eq-one?)
    (only (sph tree) tree-transform tree-contains?))

  (define load-paths
    (map ensure-trailing-slash
      (or (pass-if (getenv "SC_LOAD_PATH")
          (l (a) (string-split a #\:))) (list (getcwd)))))

  (define-as identical-infix-token list "+" "-" "<" ">" "<=" ">=" "*" "/")

  (define-as translated-infix-token list
    "equal_p"
    "or"
    "and"
    "="
    "bit_or"
    "bit_and"
    "bit_xor"
    "modulo"
    "bit_shift_right"
    "bit_shift_left"
    "eqv_p"
    "eq_p"
    "string="
    "string_equal_p")

  (define (translate-infix-token arg)
    (string-case arg
      (("=" "equal_p" "eqv_p" "eq_p" "string_equal_p" "string=") "==")
      ("and" "&&")
      ("bit_or" "|")
      ("bit_and" "&")
      ("or" "||")
      ("modulo" "%")
      ("bit_shift_right" ">>")
      ("bit_shift_left" "<<")
      ("bit_xor" "^")
      (raise (q cannot-convert-symbol-to-c))))

  (define (join-expressions arg)
    (string-join
      (fold-right
        (l (ele prev)
          (pair
            ;preprocessor directives need to start on a separate line
            (if (string-prefix? "#" ele)
              (if (or (null? prev) (not (string-prefix? "\n" (first prev))))
                (string-append "\n" ele "\n")
                (string-append "\n" ele))
              (if (or (string-suffix? "}" ele) (string-suffix? ";" ele))
                ele
                (if (string-suffix? ":" ele) (string-append ele "\n") (string-append ele ";"))))
            prev))
        (list) arg)
      ""))

  (define-syntax-rule (add-begin-if-multiple arg)
    (if (length-eq-one? arg) (first arg) (pair (q begin) arg)))

  (define (struct-or-union-body elements compile)
    (string-join
      (map
        (l (ele)
          (match ele
            ((name type ... (? integer? bits))
              (string-append
                (string-join (map sc-identifier type) " " (q suffix))
                (sc-identifier name) ":" (sc-value bits)))
            (((quote function-pointer) name type ...) (compile ele))
            ((name type ...)
              (string-append
                (string-join (map sc-identifier type) " " (q suffix))
                (sc-identifier name)))))
        elements)
      ";" (q suffix)))

  (define (ascend-expr->c arg)
    ;these are applied when ascending the tree
    (string-case (first arg)
      ("not" (string-append "!" (apply string-append (tail arg))))
      ("deref"
        (let (arg (tail arg))
          (c-pointer-deref-nc (first arg) (if (null? (tail arg)) #f (first (tail arg))))))
      ("address_of" (apply c-address-of (tail arg)))
      ("convert_type" (apply c-convert-type-nc (tail arg)))
      ("begin" (join-expressions (tail arg)))
      ("struct_ref" (apply c-struct-ref-nc (tail arg)))
      ("return" (if (null? (tail arg)) "return" (sc-apply (first arg) (tail arg))))
      ("goto" (string-join arg " "))
      ("label" (string-append (first (tail arg)) ":" (join-expressions (tail (tail arg)))))
      ("signed" (string-trim-right (first (tail arg)) #\u))
      ("length" (string-append "(8*sizeof(" (apply string-append (tail arg)) "))"))
      (identical-infix-token (parenthesise (string-join (tail arg) (first arg))))
      (translated-infix-token
        (parenthesise (string-append (string-join (tail arg) (translate-infix-token (first arg))))))
      ("define_type" (apply c-typedef-nc (tail arg)))
      ("define_function_pointer_type" (apply c-typedef-function-nc (tail arg)))
      ("bit_not" (string-append "~" (first (tail arg))))
      ("pre_stringify" (c-stringify (first (tail arg))))
      ("pre_string_concat" (string-join (tail arg) " "))
      ("compound_statement" (c-compound-nc (join-expressions (tail arg))))
      (if (list? arg) (sc-apply (first arg) (tail arg)) arg)))

  (define (descend-expr->sc arg compile)
    ;these are applied when descending the tree, and the result is parsed again
    (case (first arg)
      ((struct-set)
        (let (struct-var (first (tail arg)))
          (pair (q begin)
            (map-slice 2
              (l (field value)
                (list (q set) (list (q struct-ref) struct-var field) value))
              (tail (tail arg))))))
      ((include-sc)
        (file->datums
          (search-load-path
            (string-append (first (tail arg)) ".sc")
            load-paths)))
      ((cond cond*)
        (let
          ( (cond (reverse (tail arg)))
            (symbol-if (if (eq? (first arg) (q cond*)) (q if*) (q if))))
          (fold
            (l (cond alternate)
              (list symbol-if (first cond) (add-begin-if-multiple (tail cond)) alternate))
            (match (first cond)
              (((quote else) body ...) (add-begin-if-multiple body))
              ((test consequent ...)
                (list symbol-if test (add-begin-if-multiple consequent))))
            (tail cond))))
      (else #f)))

  (define (descend-expr->c arg compile)
    ;these are applied when descending the tree, and the result is not parsed again.
    ;this is for expressions that create syntax that can not be created with basic sc syntax
    (case (first arg)
      ((set)
        (match (tail arg)
          ((name-1 value-1 name-2 value-2 rest ...)
            (join-expressions (reverse (map-slice 2 c-set-nc (map compile (tail arg))))))
          ((name value)
            (c-set-nc
              (compile name)
              (compile value)))))
      ((define)
        (match (tail arg)
          (((name parameter ...) (return-type types ...) body ...)
            (c-function-nc
              (sc-identifier (compile name))
              (sc-identifier return-type)
              (if (null? body) #f (join-expressions (map compile body)))
              (c-parameter
                (map sc-identifier parameter)
                (map sc-identifier types))))
          (((name parameter ...) return-type body ...)
            (c-function
              (sc-identifier (compile name))
              (sc-identifier return-type)
              (if (null? body) #f (join-expressions (map compile body)))))
          ((name-1 type-1 name-2 type-2 rest ...)
            (join-expressions (reverse (map-slice 2 sc-define (tail arg)))))
          ((name type value ...)
            (c-define-nc
              (sc-identifier name)
              (sc-identifier type)
              (if (null? value) #f (compile (first value)))))))
      ((if)
        (apply c-if-statement
          (compile (first (tail arg)))
          (map (l (ele) (compile (list (q begin) ele))) (tail (tail arg)))))
      ((if-expression if*)
        (apply c-if
          (map
            (l (ele)
              (match ele
                (((quote begin) body ...)
                  (parenthesise
                    (string-join
                      (map
                        (l (ele)
                          (if (and (list? ele) (tree-contains? ele (q set)))
                            (parenthesise (compile ele))
                            (compile ele)))
                        body)
                      ",")))
                (_
                  (if (and (list? ele) (tree-contains? ele (q set)))
                    (parenthesise (compile ele))
                    (compile ele)))))
            (tail arg))))
      ((define-macro)
        (match (tail arg)
          (((name parameter ...) body ...)
            (cp-define-macro-nc (sc-identifier name)
              (string-trim-right (join-expressions (map compile body)) #\;)
              (sc-identifier-list parameter)))
          ((name-1 value-1 name-2 value-2 rest ...)
            (string-join
              (map-slice
                2 (l (name value) (cp-define-macro-nc (sc-identifier name) (compile value) #f))
                (tail arg))
              "\n"))
          ((name body ...)

            (cp-define-macro-nc (sc-identifier name) (string-trim-right (join-expressions (map compile body)) #\;) #f))))
      ((struct union)
        (let (tail-arg (tail arg))
          (apply
            (l (name body)
              (c-statement-nc (string-append (symbol->string (first arg)) name)
                (struct-or-union-body body compile)))
            (if (symbol? (first tail-arg))
              (list (string-append " " (sc-identifier (first tail-arg))) (tail tail-arg))
              (list "" tail-arg)))))
      ((struct-init)
        (string-append (c-compound-nc (string-join (map compile (tail arg)) ",")) ";"))
      ((while)
        (match (tail arg)
          ((test body ...)
            (string-append "while"
              (parenthesise (compile test))
              (c-compound-nc
                (compile (pair (q begin) body)))))))
      ((do-while)
        (match (tail arg)
          ((test body ...)
            (string-append "do"
              (c-compound-nc
                (compile (pair (q begin) body)))
              "while" (parenthesise (compile test))))))
      ((undefine-macro)
        (string-join (map (compose cp-undef sc-identifier) (tail arg)) "\n" (q suffix)))
      ((let-macro)
        ;descend-expr->sc currently would add an uneccessary semicolon at the end
        (match (tail arg)
          (((names+values ...) body ...)
            (string-append
              (string-join
                (map-slice 2
                  (l (n v) (compile (list (q define-macro) n v)))
                  names+values)
                "\n" (q suffix))
              (compile (pair (q begin) body))
              (string-join
                (map-slice 2
                  (l (n v)
                    (compile (list (q undefine-macro) (if (pair? n) (first n) n))))
                  names+values)
                "\n" (q prefix))))
          (_ (raise (q syntax-error-for-let-macro)))))
      ((include) (apply string-append (map cp-include (tail arg))))
      ((includep) (apply string-append (map cp-includep (tail arg))))
      ((function-pointer)
        (let (tail-arg (tail arg))
          (apply c-function-pointer-nc (sc-identifier (first tail-arg))
            (map
              (l (ele)
                (if (list? ele) (string-join (map sc-identifier ele) " ")
                  (sc-identifier ele)))
              (tail tail-arg)))))
      ((pre-concat) (cp-concat-nc (map sc-identifier (tail arg))))
      ((pre-if) (scp-if (q if) (tail arg) compile))
      ((pre-ifdef) (scp-if (q ifdef) (tail arg) compile))
      ((pre-ifndef) (scp-if (q ifndef) (tail arg) compile))
      ((quote) (list-ref arg 1))
      ((let*)
        (c-compound-nc
          (match (tail arg)
            ( (((names values ...) ...) body ...)
              (compile
                (pair (q begin)
                  (append
                    (map
                      (l (n v)
                        (pairs
                          (if (length-eq-one? v) (q set) (q define))
                          n v))
                      names values)
                    body)))))))
      (else #f)))

  (define (descend arg compile)
    (let (res (descend-expr->sc arg compile))
      (if res (list res #t)
        (let (res (descend-expr->c arg compile))
          (if res (list res #f) (list #f #t))))))

  (define (sc->c arg) ;expression -> string
    (tree-transform arg descend ascend-expr->c sc-value)))
