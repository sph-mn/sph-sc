;a scheme-datum to c compiler
(library (sph lang sc)
  (export sc->c sc-default-load-paths)
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
    (sph system reader)
    (only (sph filesystem) search-load-path ensure-trailing-slash)
    (only (sph read-write) file->datums)
    (only (sph list) map-slice length-eq-one?)
    (only (sph tree) tree-transform tree-contains?))

  (define sc-default-load-paths
    (map ensure-trailing-slash
      (pass-if (getenv "SC_LOAD_PATH")
        (l (a) (string-split a #\:)) (list))))

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

  (define (translate-infix-token a)
    (string-case a
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

  (define (join-expressions a)
    (string-join
      (fold-right
        (l (e prev)
          (pair
            ;preprocessor directives need to start on a separate line
            (if (string-prefix? "#" e)
              (if (or (null? prev) (not (string-prefix? "\n" (first prev))))
                (string-append "\n" e "\n")
                (string-append "\n" e))
              (if (or (string-suffix? "}" e) (string-suffix? ";" e))
                e
                (if (string-suffix? ":" e) (string-append e "\n") (string-append e ";"))))
            prev))
        (list) a)
      ""))

  (define-syntax-rule (add-begin-if-multiple a)
    (if (length-eq-one? a) (first a) (pair (q begin) a)))

  (define (struct-or-union-body elements compile)
    (string-join
      (map
        (l (e)
          (match e
            ((name type ... (? integer? bits))
              (string-append
                (string-join (map sc-identifier type) " " (q suffix))
                (sc-identifier name) ":" (sc-value bits)))
            (((quote function-pointer) name type ...) (compile e))
            ((name type ...)
              (string-append
                (string-join (map sc-identifier type) " " (q suffix))
                (sc-identifier name)))))
        elements)
      ";" (q suffix)))

  (define (ascend-expr->c a)
    ;these are applied when ascending the tree
    (string-case (first a)
      ("not" (string-append "!" (apply string-append (tail a))))
      ("deref"
        (let (a (tail a))
          (c-pointer-deref-nc (first a) (if (null? (tail a)) #f (first (tail a))))))
      ("address_of" (apply c-address-of (tail a)))
      ("convert_type" (apply c-convert-type-nc (tail a)))
      ("begin" (join-expressions (tail a)))
      ("struct_ref" (apply c-struct-ref-nc (tail a)))
      ("return" (if (null? (tail a)) "return" (sc-apply (first a) (tail a))))
      ("goto" (string-join a " "))
      ("label" (string-append (first (tail a)) ":" (join-expressions (tail (tail a)))))
      ("signed" (string-trim-right (first (tail a)) #\u))
      ("length" (string-append "(8*sizeof(" (apply string-append (tail a)) "))"))
      (identical-infix-token (parenthesise (string-join (tail a) (first a))))
      (translated-infix-token
        (parenthesise (string-append (string-join (tail a) (translate-infix-token (first a))))))
      ("define_type" (apply c-typedef-nc (tail a)))
      ("define_function_pointer_type" (apply c-typedef-function-nc (tail a)))
      ("bit_not" (string-append "~" (first (tail a))))
      ("pre_stringify" (c-stringify (first (tail a))))
      ("pre_string_concat" (string-join (tail a) " "))
      ("compound_statement" (c-compound-nc (join-expressions (tail a))))
      (if (list? a) (sc-apply (first a) (tail a)) a)))

  (define (descend-expr->sc a compile load-paths)
    ;these are applied when descending the tree, and the result is parsed again
    (case (first a)
      ((struct-set)
        (let (struct-var (first (tail a)))
          (pair (q begin)
            (map-slice 2
              (l (field value)
                (list (q set) (list (q struct-ref) struct-var field) value))
              (tail (tail a))))))
      ((include-sc)
        (pair (q begin)
          (file->datums
            (search-load-path
              (string-append (first (tail a)) ".sc")
              load-paths)
            sph-read-with-upper-case-symbols)))
      ((cond cond*)
        (let
          ( (cond (reverse (tail a)))
            (symbol-if (if (eqv? (first a) (q cond*)) (q if*) (q if))))
          (fold
            (l (cond alternate)
              (list symbol-if (first cond) (add-begin-if-multiple (tail cond)) alternate))
            (match (first cond)
              (((quote else) body ...) (add-begin-if-multiple body))
              ((test consequent ...)
                (list symbol-if test (add-begin-if-multiple consequent))))
            (tail cond))))
      (else #f)))

  (define (descend-expr->c a compile)
    ;these are applied when descending the tree, and the result is not parsed again.
    ;this is for expressions that create syntax that can not be created with basic sc syntax
    (case (first a)
      ((set)
        (match (tail a)
          ((name-1 value-1 name-2 value-2 rest ...)
            (join-expressions (reverse (map-slice 2 c-set-nc (map compile (tail a))))))
          ((name value)
            (c-set-nc
              (compile name)
              (compile value)))))
      ((define)
        (match (tail a)
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
            (join-expressions (reverse (map-slice 2 sc-define (tail a)))))
          ((name type value ...)
            (c-define-nc
              (sc-identifier name)
              (sc-identifier type)
              (if (null? value) #f (compile (first value)))))))
      ((if)
        (apply c-if-statement
          (compile (first (tail a)))
          (map (l (e) (compile (list (q begin) e))) (tail (tail a)))))
      ((if-expression if*)
        (apply c-if
          (map
            (l (e)
              (match e
                (((quote begin) body ...)
                  (parenthesise
                    (string-join
                      (map
                        (l (e)
                          (if (and (list? e) (tree-contains? e (q set)))
                            (parenthesise (compile e))
                            (compile e)))
                        body)
                      ",")))
                (_
                  (if (and (list? e) (tree-contains? e (q set)))
                    (parenthesise (compile e))
                    (compile e)))))
            (tail a))))
      ((define-macro)
        (match (tail a)
          (((name parameter ...) body ...)
            (cp-define-macro-nc (sc-identifier name)
              (string-trim-right (join-expressions (map compile body)) #\;)
              (sc-identifier-list parameter)))
          ((name-1 value-1 name-2 value-2 rest ...)
            (string-join
              (map-slice
                2 (l (name value) (cp-define-macro-nc (sc-identifier name) (compile value) #f))
                (tail a))
              "\n"))
          ((name body ...)

            (cp-define-macro-nc (sc-identifier name) (string-trim-right (join-expressions (map compile body)) #\;) #f))))
      ((struct union)
        (let (tail-a (tail a))
          (apply
            (l (name body)
              (c-statement-nc (string-append (symbol->string (first a)) name)
                (struct-or-union-body body compile)))
            (if (symbol? (first tail-a))
              (list (string-append " " (sc-identifier (first tail-a))) (tail tail-a))
              (list "" tail-a)))))
      ((struct-init)
        (string-append (c-compound-nc (string-join (map compile (tail a)) ",")) ";"))
      ((while)
        (match (tail a)
          ((test body ...)
            (string-append "while"
              (parenthesise (compile test))
              (c-compound-nc
                (compile (pair (q begin) body)))))))
      ((do-while)
        (match (tail a)
          ((test body ...)
            (string-append "do"
              (c-compound-nc
                (compile (pair (q begin) body)))
              "while" (parenthesise (compile test))))))
      ((undefine-macro)
        (string-join (map (compose cp-undef sc-identifier) (tail a)) "\n" (q suffix)))
      ((let-macro)
        ;descend-expr->sc currently would add an uneccessary semicolon at the end
        (match (tail a)
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
      ((include) (apply string-append (map cp-include (tail a))))
      ((includep) (apply string-append (map cp-includep (tail a))))
      ((function-pointer)
        (let (tail-a (tail a))
          (apply c-function-pointer-nc (sc-identifier (first tail-a))
            (map
              (l (e)
                (if (list? e) (string-join (map sc-identifier e) " ")
                  (sc-identifier e)))
              (tail tail-a)))))
      ((pre-concat) (cp-concat-nc (map sc-identifier (tail a))))
      ((pre-if) (scp-if (q if) (tail a) compile))
      ((pre-ifdef) (scp-if (q ifdef) (tail a) compile))
      ((pre-ifndef) (scp-if (q ifndef) (tail a) compile))
      ((quote) (list-ref a 1))
      ((let*)
        (c-compound-nc
          (match (tail a)
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

  (define (descend-proc load-paths)
    (l (a compile)
      (let (r (descend-expr->sc a compile load-paths))
        (if r (list r #t)
          (let (r (descend-expr->c a compile))
            (if r (list r #f) (list #f #t)))))))

  (define* (sc->c a #:optional (load-paths sc-default-load-paths)) ;expression -> string
    (tree-transform a (descend-proc load-paths) ascend-expr->c sc-value)))
