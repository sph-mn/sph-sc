(define-module (sph lang sc eval-environment))

(use-modules (srfi srfi-1) (sph list)
  (sph alist) (sph hashtable) (sph) (sph lang sc) ((sph tree) #:select (tree-any)))

(re-export any->list flatten
  contains? containsq?
  containsv? first
  second last
  map-slice map-with-index
  tail pair
  null pairs q qq debug-log sc-gensym sc-syntax? alist-ref map-integers l sc-map-associations)

(export sc-contains sc-contains-prefix sc-count-prefix sph-lang-sc-eval-environment-description)

(define sph-lang-sc-eval-environment-description
  "additional helpers available in sc-define-syntax*")

(define (sc-count-prefix a prefix)
  "count all sub-expressions in $a that begin with prefix, ignore content in sc-comment"
  (let loop ((a a) (result 0))
    (if (list? a)
      (fold
        (l (a result)
          (if (and (list? a) (not (null? a)) (not (eq? (q sc-comment) (first a))))
            (loop (tail a) (if (eq? prefix (first a)) (+ result 1) result)) result))
        result a)
      result)))

(define (sc-contains-prefix a prefix)
  "return true if any sub-expression in $a begins with prefix, ignore content in sc-comment.
   use case: conditionally add code if certain expressions were used"
  (tree-any
    (l (a)
      (and (list? a) (not (null? a)) (not (eq? (q sc-comment) (first a))) (eq? prefix (first a))))
    a))

(define (sc-contains a expression)
  "return true if any sub-expression in $a begins with prefix, ignore content in sc-comment.
   use case: conditionally add code if certain expressions were used"
  (tree-any
    (l (a)
      (and (list? a) (not (null? a)) (not (eq? (q sc-comment) (first a))) (contains? a expression)))
    a))
