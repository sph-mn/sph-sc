(library (sph lang sc check)
  (export
    sc-syntax-check
    sc-syntax-error
    sc-syntax-error?
    sc-syntax-examples
    sph-lang-sc-check-description)
  (import
    (ice-9 match)
    (rnrs exceptions)
    (sph)
    (sph alist)
    (sph lang sc expressions)
    (sph list)
    (only (sph tree) tree-every))

  (define sph-lang-sc-check-description "syntax checks, examples and error handling")

  (define (sc-syntax-examples-get name) "prepend the prefix symbol to example argument patterns"
    "symbol -> false/list"
    (and-let* ((examples (alist-ref sc-syntax-examples name)))
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
                (and-let* ((examples (sc-syntax-examples-get syntax-name)))
                  (pair (q expected) examples)))))))))

  (define* (acount? a min #:optional max)
    (let (b (length a)) (and (if min (<= min b) #t) (if max (>= max b) #t))))

  (define-as sc-syntax-examples (list->alist list-q)
    ; these are used in error messages as examples of what is allowed
    address-of ((variable))
    case
    ( (predicate subject (match-value/else consequent ...) ..1)
      (= variable (3 (return #t)) (5 (return #t)) (else (return #f))))
    declare ((name type) (name type name type name/type ...))
    define
    ( (name type value) ((name parameters ...) (return-type ...) body ...)
      ((name parameters ...) return-type body ...))
    for
    ( ( (init test update) body ...) (((init ...) test (update ...)) body ...)
      (((begin init ...) test (begin update ...)) body ...))
    pointer-get ((variable))
    array-get ((variable indices ...))
    convert-type ((variable new-type))
    if ((condition consequent) (condition consequent alternate))
    set ((variable value) (variable value variable value variable/value ...)))

  (define (sc-syntax-check-prefix-list prefix a load-paths) "list list -> boolean"
    (case prefix
      ; only arity checks
      ( (+ -* /
          array-get cond
          cond* sc-include
          function-pointer label
          pre-pragma pre-undefine
          pre-include pre-concat pre-string-concat pre-stringify sc-comment while)
        (acount? a 1))
      ( (: struct-pointer-get do-while
          bit-or bit-and bit-xor or and modulo pre-let let let* = < > <= >=)
        (acount? a 2))
      ((array-set struct-set) (acount? a 3))
      ((address-of bit-not goto not pointer-get) (acount? a 1 1))
      ((enum) (acount? a 1 2))
      ((convert-type bit-shift-right bit-shift-left) (acount? a 2 2))
      ; other checks
      ((case case*) (match a ((predicate subject clauses ..1) #t) (_ #f)))
      ((declare) (even? (length a)))
      ( (define)
        (match a
          ( ( ( (? not-preprocessor-keyword? name) parameter ...)
              ((? not-function-pointer-symbol? return-type) types ...) body ...)
            #t)
          ((((? not-preprocessor-keyword? name)) return-type body ...) #t) ((name type value) #t)
          (_ #f)))
      ((for) (match a (((init test update) body ...) #t) (_ #f)))
      ( (if if* pre-if pre-if-defined pre-if-not-defined)
        (match a ((test consequent) #t) ((test consequent alternate) #t) (_ #f)))
      ( (pre-define pre-define-if-not-defined)
        (or (acount? a 1 1) (and (even? (length a)) (acount? a 2))))
      ( (set)
        (and (even? (length a))
          (match a ((name-1 value-1 name-2 value-2 rest ...) #t) ((name value) #t) (_ #f))))
      ( (struct union)
        (match a (((? symbol?) (name type ...) ...) #t) (((name type ...) ...) #t) (_ #f)))
      (else #t)))

  (define (sc-syntax-check a load-paths) "list:expressions (string ...) -> boolean | exception"
    (tree-every
      (l (a)
        (if (and (list? a) (not (null? a)))
          (or (sc-syntax-check-prefix-list (first a) (tail a) load-paths)
            (sc-syntax-error a (first a)))
          #t))
      a)))
