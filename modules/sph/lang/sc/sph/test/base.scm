(define-module (sph lang sc sph test base))
(use-modules (sph lang sc sph) (sph lang sc sph vector))

(export test-create-result test-result
  test-result-arguments test-result-expected
  test-result-index test-result-title-set!
  test-result-assert-title-set! test-result-index-set!
  test-result-result test-result-success? test-result-title test-result? test-success?)

(define test-result-success? (vector-accessor 1))
(define test-result-title (vector-accessor 2))
(define test-result-assert-title (vector-accessor 3))
(define test-result-index (vector-accessor 4))
(define test-result-result (vector-accessor 5))
(define test-result-arguments (vector-accessor 6))
(define test-result-expected (vector-accessor 7))
(define test-result-title-set! (vector-setter 2))
(define test-result-assert-title-set! (vector-setter 3))
(define test-result-index-set! (vector-setter 4))

(define (test-result? a)
  (and (vector? a) (= 8 (vector-length a)) (eq? (q test-result) (vector-first a))))

(define* (test-create-result #:optional success? title assert-title index result arguments expected)
  "boolean string string integer any list any -> vector:test-result"
  (vector (q test-result) success? title assert-title index result arguments expected))

(define (test-success? result expected)
  "vector/any any -> boolean
   if result is a test-result, check if it is a successful result. otherwise compare result and expected for equality"
  (if (test-result? result) (test-result-success? result) (equal? result expected)))
