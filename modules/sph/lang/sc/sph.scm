(define-module (sph lang sc sph))
(use-modules (ice-9 pretty-print))
(export! let first)

(export sph-description define-syntax-rules
  define-syntax-case define-syntax-cases
  display-line l
  pair pairs
  tail each debug-log null l* q qq apply-values quote-odd quote-even quote-duplicate nullary)

(define sph-description
  "general helpers and aliases.
   # highlights
   debug-log: display trace and debug messages. (+ 3 (debug-log (+ 1 2)))
   first: alias for car
   l: alias for lambda
   null: alias for (list)
   nullary: alternative to null arity (lambda () body ...)
   pair: alias for cons
   pairs: alias for cons*
   q: alias for quote
   qq: alias for quasiquote
   quote-duplicate: a b c -> ((quote a) a (quote b) b (quote c) c)
   quote-even: a b c d -> (a (quote b) c (quote d))
   quote-odd a b c d -> ((quote a) b (quote c) d)
   tail: alias for cdr
   # syntax
   define-syntax-rules :: name ((pattern ...) expansion) ...
     similar to define-syntax-rule but for multiple patterns
   define-syntax-case :: (name pattern ...) syntax-name expansion
     similar to define-syntax-rule but for syntax-case
     removes possibility to define keywords
     removes possibility to use custom lambda for define-syntax
     removes possibility to define multiple clauses
     example
       (define-syntax-case (string-case a (condition expr ...) ...) s \"body\")
   quote-odd
     any ... -> list
     quotes each second argument starting with the first
     example: a b c d -> ((quote a) b (quote c) d)
   quote-even
     any ... -> list
     quotes each second argument starting with the second
     example: a b c d -> (a (quote b) c (quote d))
   quote-duplicate
     any ... -> list
     create two elements from each identifier: one the literal identifier symbol,
     the other the value of the variable bound to identifier.
     example: a b c -> ((quote a) a (quote b) b (quote c) c)
     example 2
       (let ((a 1) (b 2) (c 3)) (quote-duplicate a b c))
       -> (list a 1 b 2 c 3)
   nullary
     create a procedure that accepts zero arguments and evaluates body when called.
     often used for thunks
   let
     typical scheme let and named-let extended for making just one binding
     example: (let (a 3) a)")

(define-syntax-rule (define-syntax-rules name ((pattern ...) expansion) ...)
  (define-syntax name (syntax-rules () ((_ pattern ...) expansion) ...)))

(define-syntax-rules define-syntax-case
  ( ( (name . pattern) syntax-name expansion)
    (define-syntax name
      (lambda (syntax-name) (syntax-case syntax-name () ((_ . pattern) expansion)))))
  ( ( (name pattern ...) syntax-name expansion)
    (define-syntax name
      (lambda (syntax-name) (syntax-case syntax-name () ((_ pattern ...) expansion)))))
  ( ( (name . pattern) expansion)
    (define-syntax name (lambda (s) (syntax-case s () ((_ . pattern) expansion)))))
  ( ( (name pattern ...) expansion)
    (define-syntax name (lambda (s) (syntax-case s () ((_ pattern ...) expansion))))))

(define-syntax-rules define-syntax-cases
  ( (name ((pattern ...) expansion ...) ...)
    (define-syntax name
      (lambda (syntax) (syntax-case syntax () ((_ pattern ...) (begin expansion ...)) ...))))
  ( (name syntax ((pattern ...) expansion ...) ...)
    (define-syntax name
      (lambda (syntax) (syntax-case syntax () ((_ pattern ...) (begin expansion ...)) ...)))))

(define-syntax-rule (l a ...) (lambda a ...))
(define-syntax-rule (l* a ...) (lambda* a ...))
(define-syntax-rule (q a) (quote a))
(define-syntax-rule (qq a) (quasiquote a))
(define-syntax-rule (apply-values proc producer) (call-with-values (lambda () producer) proc))

(define-syntax-rules let
  ((((variable-name expr) ...) body ...) ((lambda (variable-name ...) body ...) expr ...))
  (((variable-name expr) body ...) (let ((variable-name expr)) body ...))
  ( (name ((variable-name expr) ...) body ...)
    ((lambda (name) (set! name (lambda (variable-name ...) body ...)) (name expr ...)) #f))
  ((name (variable-name expr) body ...) (let name ((variable-name expr)) body ...)))

(define-syntax-rules quote-odd (() (quote ()))
  ((a) (quote (a))) ((a b c ...) (quasiquote (a (unquote b) (unquote-splicing (quote-odd c ...))))))

(define-syntax-rules quote-even (() (quote ()))
  ((a) (quasiquote ((unquote a))))
  ((a b c ...) (quasiquote ((unquote a) b (unquote-splicing (quote-even c ...))))))

(define-syntax-rules quote-duplicate ((a) (list (quote a) a))
  ((a b ...) (quasiquote ((unquote-splicing (quote-duplicate a) (quote-duplicate b ...))))))

(define-syntax-rule (nullary body ...) (lambda () body ...))
(define first car)
(define pair cons)
(define pairs cons*)
(define tail cdr)
(define each for-each)
(define null (list))

(define* (display-line a #:optional (port (current-output-port)))
  "any [port] -> unspecified
   like \"display\" but emits a newline at the end"
  (display a port) (newline port))

(define (debug-log . a)
  "any-1 any-n ... -> any-1
   writes all arguments to standard output and returns the first argument"
  (pretty-print (cons (q --) a)) (first a))
