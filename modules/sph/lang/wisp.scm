(define-module (sph lang wisp) #:export
  (wisp-read wisp-read-all wisp-read-string wisp-read-file wisp-write wisp-write-all))

(use-modules (sph) (ice-9 peg) (ice-9 match) (srfi srfi-1))
(define-peg-pattern nl none "\n")
(define-peg-pattern indent body (* (or " " "_")))
(define-peg-pattern dq-esc none (and "\\" peg-any))
(define-peg-pattern dq-char body (or dq-esc (and (not-followed-by "\"") peg-any)))
(define-peg-pattern dq-string body (and "\"" (* dq-char) "\""))

(define-peg-pattern paren-group body
  (and "("
    (* (or dq-string paren-group bracket-group brace-group (and (not-followed-by ")") peg-any))) ")"))

(define-peg-pattern bracket-group body
  (and "["
    (* (or dq-string paren-group bracket-group brace-group (and (not-followed-by "]") peg-any))) "]"))

(define-peg-pattern brace-group body
  (and "{"
    (* (or dq-string paren-group bracket-group brace-group (and (not-followed-by "}") peg-any))) "}"))

(define-peg-pattern segment body (or dq-string paren-group bracket-group brace-group))
(define-peg-pattern comment none (and ";" (* (and (not-followed-by nl) peg-any))))

(define-peg-pattern payload body
  (* (or segment (and (not-followed-by nl) (not-followed-by ";") peg-any))))

(define-peg-pattern line all (and indent payload (* comment) (+ nl)))
(define-peg-pattern chunk all (and (+ line) (not-followed-by peg-any)))

(define (prefix->repr prefix-chars)
  (match prefix-chars ((#\.) (quote dot))
    ((#\') (quote quote)) ((#\, #\@) (quote unquote-splicing))
    ((#\,) (quote unquote)) ((#\`) (quote quasiquote))
    ((#\# #\') (quote syntax)) ((#\# #\, #\@) (quote unsyntax-splicing))
    ((#\# #\,) (quote unsyntax)) ((#\# #\`) (quote quasisyntax)) (_ #f)))

(define (read-token input-port)
  (let loop ((accumulator (quote ())))
    (cond
      ( (or (eof-object? (peek-char input-port)) (<= 4 (length accumulator))
          (memv (peek-char input-port) (quote (#\space #\newline))))
        (let ((repr (prefix->repr (reverse accumulator))))
          (cond
            (repr repr)
            ((null? accumulator) (read input-port))
            (else (for-each (lambda (c) (unread-char c input-port)) accumulator) (read input-port)))))
      (else (loop (cons (read-char input-port) accumulator))))))

(define (collect-lines parse-tree)
  (cond
    ((and (pair? parse-tree) (eq? (car parse-tree) (quote line))) (list parse-tree))
    ((pair? parse-tree) (append (collect-lines (car parse-tree)) (collect-lines (cdr parse-tree))))
    (else (quote ()))))

(define (count-indent line-string)
  (let ((len (string-length line-string)))
    (let loop ((index 0))
      (if
        (and (< index len)
          (let ((ch (string-ref line-string index))) (or (char=? ch #\space) (char=? ch #\_))))
        (loop (+ index 1)) index))))

(define (peg->lines input-string)
  (let ((match (match-pattern chunk input-string)))
    (if (not match) (quote ())
      (map
        (lambda (node)
          (let*
            ( (string-parts (filter string? (cdr node)))
              (full-line (if (null? string-parts) "" (apply string-append string-parts)))
              (k (count-indent full-line)))
            (list k (substring full-line k))))
        (collect-lines (peg:tree match))))))

(define (string->tokens line-string)
  (let loop ((port (open-input-string line-string)) (tokens (quote ())))
    (let ((tok (read-token port)))
      (if (eof-object? tok) (reverse tokens) (loop port (cons tok tokens))))))

(define (inline-colons tokens)
  (let loop ((acc (quote ())) (rest tokens))
    (cond
      ((null? rest) (reverse acc))
      ((eq? (car rest) (quote :)) (reverse (cons (loop (quote ()) (cdr rest)) acc)))
      (else (loop (cons (car rest) acc) (cdr rest))))))

(define (strip-lone-colon tokens)
  (if (and (= 1 (length tokens)) (eq? (car tokens) (quote :))) (quote ()) tokens))

(define (continues? tokens) (and (pair? tokens) (eq? (car tokens) (quote dot))))
(define (strip-cont tokens) (if (continues? tokens) (cdr tokens) tokens))

(define (prepare-lines indented-lines)
  (map
    (lambda (line-pair)
      (let*
        ( (indent (car line-pair)) (text (cadr line-pair)) (raw-tokens (string->tokens text))
          (tokens (inline-colons (strip-lone-colon raw-tokens))))
        (list indent tokens (continues? tokens))))
    indented-lines))

(define (frame-indent frame) (list-ref frame 0))
(define (frame-tokens frame) (list-ref frame 1))
(define (frame-children frame) (list-ref frame 2))
(define (make-frame indent tokens) (list indent tokens (quote ())))
(define (with-frame-tokens frame tokens) (list (frame-indent frame) tokens (frame-children frame)))

(define (push-child frame child)
  (list (frame-indent frame) (frame-tokens frame) (append (frame-children frame) (list child))))

(define (finalize-frame frame) (append (frame-tokens frame) (frame-children frame)))

(define (close-while> stack target-indent)
  (let loop ((st stack))
    (if (or (null? st) (<= (frame-indent (car st)) target-indent)) st
      (let* ((top (car st)) (rest (cdr st)) (expr (finalize-frame top)))
        (if (null? rest) (cons (make-frame -1 (list expr)) (quote ()))
          (loop (cons (push-child (car rest) expr) (cdr rest))))))))

(define (close-equal stack)
  (if (or (null? stack) (null? (cdr stack))) stack
    (let* ((top (car stack)) (parent (cadr stack)) (rest (cddr stack)) (expr (finalize-frame top)))
      (cons (push-child parent expr) rest))))

(define (parenize prepared-lines)
  (let loop ((stack (quote ())) (accumulated (quote ())) (remaining prepared-lines))
    (cond
      ( (null? remaining)
        (let close-all ((st stack) (acc accumulated))
          (if (null? st) acc
            (let* ((top (car st)) (rest (cdr st)) (expr (finalize-frame top)))
              (if (null? rest)
                (close-all rest
                  (append acc (if (= -1 (frame-indent top)) (frame-tokens top) (list expr))))
                (close-all (cons (push-child (car rest) expr) (cdr rest)) acc))))))
      (else
        (let*
          ( (line (car remaining)) (indent (list-ref line 0))
            (tokens (strip-cont (list-ref line 1))) (cont? (list-ref line 2)))
          (if (null? tokens) (loop stack accumulated (cdr remaining))
            (if cont?
              (let
                ( (stack
                    (if (pair? stack)
                      (cons
                        (with-frame-tokens (car stack) (append (frame-tokens (car stack)) tokens))
                        (cdr stack))
                      (list (make-frame indent tokens)))))
                (loop stack accumulated (cdr remaining)))
              (let*
                ( (stack (close-while> stack indent))
                  (stack
                    (if (and (pair? stack) (= indent (frame-indent (car stack))))
                      (close-equal stack) stack))
                  (stack (cons (make-frame indent tokens) stack)))
                (loop stack accumulated (cdr remaining))))))))))

(define (wisp->sexp text) (parenize (prepare-lines (peg->lines text))))

(define (read-chunk-text input-port)
  (let read-loop ((acc "") (nl-count 0))
    (if (or (eof-object? (peek-char input-port)) (>= nl-count 2)) acc
      (let ((ch (read-char input-port)))
        (cond
          ((eof-object? ch) acc)
          ((char=? ch #\newline) (read-loop (string-append acc (string ch)) (+ nl-count 1)))
          (else (read-loop (string-append acc (string ch)) 0)))))))

(define (wisp-read input-port) (set-port-encoding! input-port "UTF-8")
  (if (eof-object? (peek-char input-port)) (read-char input-port)
    (let* ((chunk (read-chunk-text input-port)) (forms (wisp->sexp chunk)))
      (and (pair? forms) (car forms)))))

(define (wisp-read-all input-port)
  (let loop ((out (quote ())))
    (if (eof-object? (peek-char input-port)) out
      (let* ((chunk (read-chunk-text input-port)) (forms (wisp->sexp chunk)))
        (loop (append out forms))))))

(define (wisp-read-string string) (call-with-input-string string wisp-read-all))
(define (wisp-read-file path) (call-with-input-file path wisp-read-all))

(define (wisp-write expr port)
  (let rec ((form expr) (col 0))
    (cond
      ( (pair? form)
        (match form (((quote quote) a) (display "' " port) (rec a col))
          (((quote quasiquote) a) (display "` " port) (rec a col))
          (((quote unquote) a) (display ", " port) (rec a col))
          (((quote unquote-splicing) a) (display ",@ " port) (rec a col))
          (else (display (make-string col #\space) port) (rec (car form) col)
            (for-each (lambda (e) (newline port) (rec e (+ col 2))) (cdr form)))))
      (else (display (make-string col #\space) port) (write form port)))))

(define (wisp-write-all exprs port)
  (let loop ((rest exprs))
    (unless (null? rest) (wisp-write (car rest) port)
      (newline port) (newline port) (loop (cdr rest)))))
