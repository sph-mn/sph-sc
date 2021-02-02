(define-module (sph lang sc eval-environment))
(use-modules (srfi srfi-1) (sph list) (sph alist) (sph hashtable) (sph) (sph lang sc))

(re-export any->list flatten
  contains? containsq?
  containsv? first
  map-with-index tail pair null pairs q qq debug-log sc-gensym sc-syntax? alist-ref map-integers l)

(export sc-count-expressions sph-lang-sc-eval-environment-description)

(define sph-lang-sc-eval-environment-description
  "additional helpers available in sc-define-syntax*")

(define (sc-count-expressions prefix a)
  "count all sub-expressions in a that begin with prefix, ignore content in sc-comment"
  (let loop ((a a) (result 0))
    (if (list? a)
      (fold
        (l (a result)
          (if (and (list? a) (not (null? a)) (not (eq? (q sc-comment) (first a))))
            (loop (tail a) (if (eq? prefix (first a)) (+ result 1) result)) result))
        result a)
      result)))
