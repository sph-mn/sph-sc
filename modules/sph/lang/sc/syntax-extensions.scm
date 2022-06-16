(define-module (sph lang sc syntax-extensions))
(export sc-syntax-extensions)

(define sc-syntax-extensions
  (quasiquote
    ( (sc-define-syntax (1+ x) (+ x 1)) (sc-define-syntax (1- x) (- x 1))
      (sc-define-syntax (when test-expr body ...) (if test-expr (begin body ...)))
      (sc-define-syntax (unless test-expr body ...) (when (not test-expr) body ...))
      (sc-define-syntax (for-each-index index type limit body ...)
        (for ((define index type 0) (< index limit) (set+ index 1)) body ...)))))
