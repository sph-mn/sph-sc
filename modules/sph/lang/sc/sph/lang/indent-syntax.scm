(define-module (sph lang sc sph lang indent-syntax) #:export
  (denoted-tree->indent-tree denoted-tree->indent-tree-lines indent-tree->denoted-tree
    indent-tree->prefix-tree prefix-tree->indent-tree
    prefix-tree-text read-indent-tree->denoted-tree
    read-indent-tree->prefix-tree read-indent-tree-element->denoted-tree
    string->indent-depth line->indent-and-content indent-tree->range-delimited-tree))

(use-modules (rnrs io ports) (srfi srfi-1) (sph lang sc sph) (sph lang sc sph lang indent-tree))

(define (string-repeat s n)
  (let loop ((k n) (acc "")) (if (<= k 0) acc (loop (- k 1) (string-append acc s)))))

(define (string->indent-depth s indent-width)
  (let ((n (string-skip s #\space))) (and n (quotient (+ n (- indent-width 1)) indent-width))))

(define (line->indent-and-content s indent-width)
  (let ((n (string-skip s #\space)))
    (if n
      (cons (quotient (+ n (- indent-width 1)) indent-width)
        (string-trim-right (string-drop s n) #\space))
      (cons 0 s))))

(define* (indent-tree->denoted-tree a #:optional (indent-width 2))
  (read-indent-tree->denoted-tree (open-input-string a) indent-width))

(define* (read-indent-tree->denoted-tree port #:optional (indent-width 2))
  (read-indent-pairs/width (get-string-all port) indent-width))

(define* (denoted-tree->indent-tree-lines xs #:optional (base-depth 0) (indent-string "  "))
  (map (lambda (pr) (string-append (string-repeat indent-string (+ base-depth (car pr))) (cdr pr)))
    xs))

(define* (denoted-tree->indent-tree xs #:optional (base-depth 0) (indent-string "  "))
  (string-join (denoted-tree->indent-tree-lines xs base-depth indent-string) "\n"))

(define (denoted->prefix xs)
  (let loop ((rest xs) (acc (quote ())) (cur 0))
    (cond
      ((null? rest) (reverse acc))
      ((> (caar rest) cur) (loop rest (cons (quote ()) acc) (+ cur 1)))
      ((< (caar rest) cur) (loop rest (cons (quote UP) acc) (- cur 1)))
      (else (loop (tail rest) (cons (cdar rest) acc) cur)))))

(define (normalize-prefix xs)
  (let loop ((rest (reverse xs)) (stack (quote (()))))
    (if (null? rest) (first stack)
      (let ((x (first rest)))
        (cond
          ((eq? x (quote UP)) (loop (tail rest) (cons (quote ()) stack)))
          ((list? (first stack)) (loop (tail rest) (cons (cons x (first stack)) (tail stack))))
          (else (loop (tail rest) stack)))))))

(define (denoted-tree->prefix-tree xs) (normalize-prefix (denoted->prefix xs)))

(define* (indent-tree->prefix-tree a #:optional (indent-width 2))
  (denoted-tree->prefix-tree (indent-tree->denoted-tree a indent-width)))

(define (prefix-tree->denoted-tree xs)
  (define (emit n d)
    (cond
      ((string? n) (list (cons d n)))
      ( (pair? n)
        (let ((h (car n)) (t (cdr n)))
          (cond
            ( (and (string? h) (pair? t))
              (append (list (cons d h)) (apply append (map (lambda (c) (emit c (+ d 1))) t))))
            ((and (string? h) (null? t)) (emit h (+ d 1)))
            (else (apply append (map (lambda (c) (emit c (+ d 1))) n))))))
      (else (quote ()))))
  (apply append (map (lambda (n) (emit n 0)) xs)))

(define* (prefix-tree->indent-tree xs #:optional (base-depth 0) (indent-string "  "))
  (denoted-tree->indent-tree (prefix-tree->denoted-tree xs) base-depth indent-string))

(define (prefix-tree-text xs) (prefix-tree->indent-tree xs))

(define (indent-tree->range-delimited-tree s indent-char indent-width start-delim end-delim)
  (let*
    ( (pairs (read-indent-pairs/width s indent-width))
      (out
        (let loop ((rest pairs) (prev 0) (acc (quote ())))
          (if (null? rest)
            (if (> prev 0)
              (string-append (apply string-append (reverse acc))
                (make-string (* prev (string-length end-delim)) (string-ref end-delim 0)))
              (apply string-append (reverse acc)))
            (let*
              ( (d (caar rest)) (t (cdar rest))
                (open (if (> d prev) (make-list (- d prev) start-delim) (quote ())))
                (close (if (< d prev) (make-list (- prev d) end-delim) (quote ()))))
              (loop (tail rest) d
                (cons (string-append (apply string-append close) (apply string-append open) t "\n")
                  acc)))))))
    out))

(define* (read-indent-tree-element->denoted-tree port #:optional (indent-width 2))
  (let* ((pairs (read-indent-tree->denoted-tree port indent-width)))
    (let loop ((rest pairs) (acc (quote ())))
      (cond
        ((null? rest) (reverse acc))
        ((= 0 (caar rest)) (reverse acc))
        (else (loop (tail rest) (cons (list (caar rest) (cdar rest)) acc)))))))
