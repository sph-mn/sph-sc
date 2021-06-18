(define-module (sph lang sc syntax-extensions))
(export sc-syntax-extensions)

(define sc-syntax-extensions
  (quasiquote
    ( (sc-define-syntax (1+ x) (+ x 1))
      (sc-define-syntax (1- x) (- x 1))
      (sc-define-syntax (when test-expr body ...) (if test-expr (begin body ...)))
      (sc-define-syntax (unless test-exprx body ...) (when (not test-exprx) body ...)))))
