(define-module (sph lang sc sph lang scheme))

(use-modules (rnrs eval) (rnrs io ports)
  ((srfi srfi-1) #:select (remove)) (sph lang sc sph) (sph lang sc sph alist) (sph lang sc sph hashtable) (sph lang sc sph io) (sph lang sc sph tree))

(export file->datums iq-file
  iq-file-hashtable iq-file-lines port->datums string->datum string->datums)

(define sph-lang-scheme-description
  "scheme parsing helpers including helpers for implicitly quasiquoted configuration files")

(define* (iq-file path #:optional (env (current-module)))
  "string -> list
   \"implicitly quasiquoted\".
   read all scheme expressions from file like elements of a quasiquoted
   list (quasiquote (file-content-expression ...)).
   unquote can be used and will be evaluated.
   example use case: configuration files"
  (eval (list (q quasiquote) (file->datums path)) env))

(define* (iq-file-lines path #:optional (env (current-module)))
  "string -> ((line-expressions ...) ...)
   like iq-file but keep the expressions of lines in separate lists"
  (eval
    (list (q quasiquote)
      (remove null? (map string->datums (call-with-input-file path port->lines))))
    env))

(define (iq-file-hashtable path)
  "string -> list
   read file like iq-file and then convert the resulting list to an association list
   by interpreting elements as key and value alternatingly"
  (tree-map-lists-self (compose ht-from-alist list->alist) (iq-file path)))

(define* (string->datum a #:optional (reader read)) "get the first scheme expression from a string"
  (call-with-input-string a reader))

(define* (string->datums a #:optional (reader read))
  "string -> list
   get all scheme expression from a string"
  (let (a (open-input-string a))
    (let loop () (let (b (reader a)) (if (eof-object? b) (list) (pair b (loop)))))))

(define* (port->datums port #:optional (get-datum get-datum))
  "string procedure:reader -> list
   read all scheme datums from a port"
  (let loop ((a (get-datum port))) (if (eof-object? a) (list) (pair a (loop (get-datum port))))))

(define* (file->datums path #:optional (get-datum get-datum))
  "string procedure:reader -> list
   read all scheme datums of a file specified by path"
  (call-with-input-file path (l (port) (port->datums port get-datum))))
