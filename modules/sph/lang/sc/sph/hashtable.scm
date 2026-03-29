(define-module (sph lang sc sph hashtable))

(use-modules (sph lang sc sph) (sph lang sc sph string)
  (srfi srfi-1) ((sph lang sc sph list) #:select (map-slice))
  ((sph lang sc sph vector) #:select (vector-each vector-each-with-index)) ((rnrs hashtables) #:prefix rnrs-))

(export ht-list-replace ht-alist ht-bind
  ht-clear! ht-contains?
  ht-copy ht-copy*
  ht-copy-empty ht-create
  ht-create-binding ht-create-eq
  ht-create-eqv ht-create-string
  ht-create-symbol ht-create-symbol-q
  ht-delete! ht-each
  ht-each-key ht-entries
  ht-equivalence-function ht-fold
  ht-fold-right ht-from-alist
  ht-from-list ht-from-tree
  ht-hash-equal ht-hash-function
  ht-hash-string ht-hash-symbol
  ht-invert! ht-keys
  ht-make ht-make-eq
  ht-make-eqv ht-make-string
  ht-map! ht-merge!
  ht-object ht-ref
  ht-ref-q ht-select
  ht-set! ht-set-multiple!
  ht-set-multiple-q! ht-set-q!
  ht-size ht-tree-and-ref
  ht-tree-and-ref-q ht-tree-contains?
  ht-tree-copy ht-tree-copy*
  ht-tree-merge! ht-tree-ref
  ht-tree-ref-q ht-tree-set! ht-update-multiple! ht-values ht? sph-hashtable-description)

(define sph-hashtable-description
  "rnrs-hashtable processing.
   syntax
     ht-bind :: hashtable (key ...) body ... -> any
       selectively bind keys of hashtable to variables.
       keys are unquoted literals.
       example: (ht-bind my-ht (a b c) (+ 1 c b a))
     ht-ref :: hashtable key [default] -> any
       rnrs ht-ref with an optional default argument
     ht-ref-q :: hashtable hashtable key -> any
       key is implicitly quoted
     ht-create-symbol :: associations ...
       create a hashtable optimised for symbol keys
     ht-bind :: values ...
       equivalent to (ht-create-symbol (q name) name ...)
     ht-create-eq
       create a hashtable that uses eq? as a comparison function")

(define ht-set! rnrs-hashtable-set!)
(define ht-equivalence-function rnrs-hashtable-equivalence-function)
(define ht-hash-function rnrs-hashtable-hash-function)
(define ht-size rnrs-hashtable-size)
(define ht-entries rnrs-hashtable-entries)
(define ht-keys rnrs-hashtable-keys)
(define ht-delete! rnrs-hashtable-delete!)
(define ht-clear! rnrs-hashtable-clear!)
(define ht-hash-string rnrs-string-hash)
(define ht-hash-symbol rnrs-symbol-hash)
(define ht-hash-equal rnrs-equal-hash)
(define ht-contains? rnrs-hashtable-contains?)
(define ht? rnrs-hashtable?)
(define ht-copy rnrs-hashtable-copy)
(define ht-make rnrs-make-hashtable)
(define ht-make-eq rnrs-make-eq-hashtable)
(define ht-make-eqv rnrs-make-eqv-hashtable)

(define* (ht-make-string #:optional size)
  (if size (rnrs-make-hashtable ht-hash-string string-equal? size)
    (rnrs-make-hashtable ht-hash-string string-equal?)))

(define-syntax-rules ht-ref ((h k d) (rnrs-hashtable-ref h k d))
  ((h k) (rnrs-hashtable-ref h k #f)))

(define-syntax-rules ht-ref-q ((h k d) (ht-ref h (quote k) d)) ((h k) (ht-ref h (quote k) #f)))
(define-syntax-rule (ht-set-q! h k v) (ht-set! h (quote k) v))

(define-syntax-rules ht-tree-ref ((h k) (ht-ref h k #f))
  ((h k ... k-last) (ht-ref (ht-tree-ref h k ...) k-last #f)))

(define-syntax-rule (ht-tree-ref-q a key ...) (ht-tree-ref a (quote key) ...))

(define-syntax-rules ht-tree-and-ref ((h k) (ht-ref h k #f))
  ((h k ... k-last) (let (a (ht-tree-ref h k ...)) (and a (ht-ref a k-last #f)))))

(define-syntax-rule (ht-tree-and-ref-q a key ...) (ht-tree-and-ref a (quote key) ...))

(define-syntax-rules ht-tree-set! ((h k v) (ht-set! h k v))
  ((h k ... k-last v) (ht-set! (ht-ref h k ...) k-last v)))

(define-syntax-rules ht-create-symbol (() (ht-make ht-hash-symbol eq?))
  ((associations ...) (ht-from-list (list associations ...) eq? ht-hash-symbol)))

(define-syntax-rules ht-create-symbol-q (() (ht-make ht-hash-symbol eq?))
  ((associations ...) (ht-from-list (quote-odd associations ...) eq? ht-hash-symbol)))

(define-syntax-rules ht-create-binding (() (ht-make ht-hash-symbol eq?))
  ((binding-name ...) (ht-from-list (quote-duplicate binding-name ...) eq? ht-hash-symbol)))

(define-syntax-rules ht-create-string (() (ht-make ht-hash-string string-equal?))
  ((associations ...) (ht-from-list (list associations ...) string-equal? ht-hash-string)))

(define-syntax-rules ht-create-eq (() (ht-make-eq))
  ((associations ...) (ht-from-list (quote-odd associations ...) eq? ht-hash-equal)))

(define-syntax-rules ht-create-eqv (() (ht-make-eqv))
  ((associations ...) (ht-from-list (quote-odd associations ...) eqv? ht-hash-equal)))

(define-syntax-rule (ht-bind ht (key ...) body ...)
  ((lambda (key ...) body ...) (ht-ref ht (quote key)) ...))

(define-syntax-rule (ht-each-key proc ht) (vector-each proc (ht-keys ht)))

(define-syntax-rule (ht-set-multiple-q! a key/value ...)
  "hashtable [any:unquoted-key any:value] ..." (apply ht-set-multiple! a (quote-odd key/value ...)))

(define (ht-tree-contains? a . keys)
  (let loop ((a (ht-ref a (first keys) (q --not-contained))) (keys (tail keys)))
    (and (not (eq? (q --not-contained) a))
      (or (null? keys) (loop (ht-ref a (first keys)) (tail keys))))
    a keys))

(define* (ht-from-alist a #:key (equal-f equal?) (hash-f ht-hash-equal) (depth 0))
  "list #:equal-f procedure #:hash-f procedure #:depth integer/infinite -> hashtable
   convert alist 'a' to an r6rs hashtable.
   if depth is positive then also convert nested alists to hashtables at most depth nestings deep.
   depth can be (inf)"
  (let (ht (ht-make hash-f equal-f))
    (each
      (l (a)
        (let (b (tail a))
          (ht-set! ht (first a)
            (if (or (= 0 depth) (not (and (list? b) (every pair? b)))) b
              (ht-from-alist b #:equal-f equal-f #:hash-f hash-f #:depth (- depth 1))))))
      a)
    ht))

(define (ht-create . associations)
  "{key value} ... -> hashtable
   creates a hashtable.
   example: (hashtable 'a 1 'b 2 'c 3)"
  (ht-from-list associations))

(define (ht-each proc ht)
  "procedure:{key value ->} hashtable ->
   call proc for each key and value association in hashtable"
  (apply-values
    (l (keys values) (vector-each-with-index (l (index a) (proc a (vector-ref values index))) keys))
    (ht-entries ht)))

(define (ht-values a) "hashtable -> vector"
  (call-with-values (nullary (ht-entries a)) (l (keys values) values)))

(define (ht-fold proc init a) "procedure:{key value state -> state} any hashtable -> list"
  (apply-values (l (keys values) (fold proc init (vector->list keys) (vector->list values)))
    (ht-entries a)))

(define (ht-fold-right proc init a) "procedure:{key value state -> state} any hashtable -> list"
  (apply-values (l (keys values) (fold-right proc init (vector->list keys) (vector->list values)))
    (ht-entries a)))

(define (ht-map! proc a) "hashtable -> hashtable" (ht-each (l (k v) (ht-set! a k (proc k v))) a))

(define (ht-invert! a)
  "hashtable -> hashtable
   use values as keys and keys as values"
  (ht-each (l (k v) (ht-set! a v k)) a))

(define (ht-merge! a . b)
  "hashtable hashtable ... -> unspecified
   copy the values of hash b to hash a. existing key values are overwritten"
  (each
    (l (b)
      (apply-values
        (l (keys values)
          (vector-each-with-index (l (index key) (ht-set! a key (vector-ref values index))) keys))
        (ht-entries b)))
    b))

(define (ht-tree-merge! a . b)
  "hashtable ... -> unspecified
   merges hashtables b from right to left into a. nested hashtables are merged recursively"
  (each
    (l (b)
      (apply-values
        (l (keys values)
          (vector-each-with-index
            (l (index key)
              (let ((a-value (ht-ref a key)) (b-value (vector-ref values index)))
                (ht-set! a key
                  (if (and (ht? a-value) (ht? b-value))
                    (begin (ht-tree-merge! a-value b-value) a-value) b-value))))
            keys))
        (ht-entries b)))
    (reverse b)))

(define (ht-set-multiple! ht . assoc)
  "hashtable key/value ...
   return a new hashtable with multiple values having been added or updated"
  (map-slice 2 (l (key value) (ht-set! ht key value)) assoc))

(define (ht-update-multiple! ht keys proc)
  "hashtable list procedure:{any:values ... -> (any:new-values ...)} -> hashtable
   set values for 'keys' in hashtable to new values by mapping using 'proc'"
  (each (l (k v) (ht-set! ht k v)) keys (apply proc (map (l (a) (ht-ref ht a)) keys))))

(define* (ht-alist ht #:optional (depth 0))
  "rnrs-hashtable [integer] -> list
   converts a hashtable to an alist. if depth is greater than 0 any other
   hashtables being values up to this nesting depth will be converted too.
   scheme has a value for infinite that can be used as depth"
  (ht-fold
    (l (key value r)
      (pair (pair key (if (and (> depth 0) (ht? value)) (ht-alist value (- depth 1)) value)) r))
    (list) ht))

(define* (ht-from-list a #:optional (equal-proc equal?) (hash-proc ht-hash-equal))
  "convert a list to an r6rs standard library hashtable. nested lists are not converted to a hash.
   example
   (ht-ref (ht-from-list (list 'a 1 'b 2)) 'b #f)
   -> 2"
  (let (result (ht-make hash-proc equal-proc))
    (fold (l (value key) (if key (begin (ht-set! result key value) #f) value)) #f a) result))

(define* (ht-from-tree a #:optional (equal-proc equal?) (hash-proc ht-hash-equal))
  "list [procedure:{a b -> boolean} procedure] -> rnrs-hashtable
   like ht-from-list but also converts nested lists to nested hashtables"
  (let (result (ht-make hash-proc equal-proc))
    (fold (l (value key) (if key (begin (ht-set! result key value) #f) value)) #f a) result))

(define (ht-select a keys) (map (l (b) (ht-ref a b)) keys))

(define (ht-copy-empty a)
  "hashtable -> hashtable
   creates a new empty hashtable with the same equivalence and hash function as the input hashtable."
  (ht-make (ht-hash-function a) (ht-equivalence-function a)))

(define (ht-copy* a f) "call f with a copy of hashtable and return it"
  (let (r (ht-copy a #t)) (f r) r))

(define (ht-tree-copy a)
  (ht-fold (l (k v r) (ht-set! r k (if (ht? v) (ht-tree-copy v) v)) r) (ht-copy-empty a) a))

(define (ht-tree-copy* a proc) (let (r (ht-tree-copy a)) (proc r) r))

(define* (ht-object a #:optional default)
  "hashtable [any:default] -> procedure:{any:key -> any:value/default}"
  (l (key) (ht-ref a key default)))

(define (ht-list-replace a ht)
  "list rnrs-hashtable -> list
   replaces elements in list that exist as key in a hashtable with the associated value.
   if the value is a list, the element is either removed (empty list) or replaced with multiple elements"
  (fold
    (l (e r)
      (let (value (ht-ref ht e)) (if value ((if (list? value) append pair) value r) (pair e r))))
    (list) a))
