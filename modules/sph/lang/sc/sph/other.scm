(define-module (sph lang sc sph other))

(use-modules (srfi srfi-1) ((rnrs exceptions) #:select (guard))
  (ice-9 pretty-print) (ice-9 rdelim)
  (ice-9 regex) (rnrs bytevectors)
  (rnrs io ports) (rnrs sorting)
  (sph lang sc sph) (sph lang sc sph hashtable) (sph lang sc sph list) (sph lang sc sph math) (sph lang sc sph number) (sph lang sc sph string) (sph lang sc sph vector))

(export begin-first boolean->integer
  boolean-true? call-at-approximated-interval
  call-at-interval call-at-interval-w-state
  char-set->vector cli-option
  cli-option-join each-integer
  false-if false-if-not
  guile-exception->key identity-if
  if-pass if-pass-apply
  if-predicate-and if-predicate-or
  ignore pass
  pass-predicate-and-if pass-predicate-or-if
  predicate-and predicate-or
  procedure->cached-procedure procedure->temporarily-cached-procedure
  procedure-append procedure-append*
  procedure-append-ignore-result procedure-cond
  program-name program-path
  random-ascii-string random-between
  random-boolean random-bytevector
  random-chars random-discrete-f
  random-string remove-keyword-associations
  repeat rnrs-exception->key
  rnrs-exception->object search-env-path
  search-env-path-one socket-bind sph-other-description values->list)

(define sph-other-description
  "miscellaneous.
   # syntax
   define-as
     example: (define-as list 1 2 3)
     example: (define-as (quasiquote list) 1 (unquote 3))
   compose-s
     (compose-s a (list 1 2)) -> (a (list 1 2))
     (compose-s (a b) (list 1 2)) -> (a (b (list 1 2)))
     it does not fail in the case (_ (quasiquote list) expr ...)
   begin-first :: result expression ...
     like begin but returns the result of the first expression instead of the last one
   identity-if :: result-if-true else ...
     give the result of result-if-true, otherwise execute else
   procedure-cond :: a (predicate handler) ... else
     similar to \"cond\" but with procedures for predicate and handler.
     passes the result to predicate, and if it evaluates to true then it passes the result to handler and the result
     will be the result of handler. if predicate evaluates to false, the next predicate is checked.
     if no predicate matches, the result of procedure-cond is the result of the last expression
   values->list :: producer
     converts multiple values to a list.
     example: (values->list (values 1 2 3)) -> (1 2 3)
   if-pass
     any procedure:{any -> any} -> any
     call proc with \"a\" if \"a\" is a true value, otherwise return false or evaluate else.
     also known as \"and=>\"
   if-pass-apply
     list procedure:{any ... -> any} -> any
     like if-pass but uses apply to use the contents of \"a\", which should be a list in the true case, as arguments to proc")

(define-syntax-case (compose-s name expr ...) s
  (let (name-datum (syntax->datum (syntax name)))
    (if (list? name-datum)
      (let (name-datum (reverse name-datum))
        (datum->syntax s
          (fold list (pair (first name-datum) (syntax->datum (syntax (expr ...))))
            (tail name-datum))))
      (syntax (name expr ...)))))

(define-syntax-rules define-as
  ((name (wrap-name ...) expr ...) (define name (compose-s (wrap-name ...) expr ...)))
  ((name wrap-name expr ...) (define name (wrap-name expr ...))))

(define (cusum a . b) (pair a (if (null? b) null (apply cusum (+ a (first b)) (tail b)))))
(define-syntax-rule (values->list producer) (call-with-values (l () producer) list))

(define-syntax-rule (procedure-cond a (predicate handler) ... else)
  "should probably have an else keyword case instead of returning the last expression"
  (let (b a) (cond ((predicate b) (handler b)) ... else)))

(define-syntax-rule (begin-first result expression ...) ((l (a) expression ... a) result))
(define-syntax-rule (any->list a) (if (list? a) a (list a)))

(define-syntax-rule (identity-if result-if-true else ...)
  ((lambda (r) (if r r (begin else ...))) result-if-true))

(define-syntax-rule (false-if test consequent)
  "result in false if \"test\" is true, otherwise execute consequent" (if test #f consequent))

(define-syntax-rule (false-if-not test consequent)
  "execute consequent if \"test\" is true, otherwise result in false" (if test consequent #f))

(define (ignore . a)
  "any ... -> unspecified
   ignores all arguments and returns unspecified"
  (if #f #t))

(define-syntax-rules if-predicate-and
  ( ( (predicate ...) subject consequent alternative)
    (let (b subject) (if (and (predicate b) ...) consequent alternative)))
  ( (predicate subject consequent alternative)
    (if-predicate-and (predicate) subject consequent alternative)))

(define-syntax-rules if-predicate-or
  ( ( (predicate ...) subject consequent alternative)
    (let (b subject) (if (or (predicate b) ...) consequent alternative)))
  ( (predicate subject consequent alternative)
    (if-predicate-or (predicate) subject consequent alternative)))

(define-syntax-rules if-pass
  ((a consequent alternative) (let (b a) (if b (consequent b) alternative)))
  ((a consequent) (let (b a) (if b (consequent b) b))))

(define-syntax-rules if-pass-apply
  ((a consequent alternative) (let (b a) (if b (apply consequent b) alternative)))
  ((a consequent) (let (b a) (if b (apply consequent b) b))))

(define-syntax-rules boolean-true? ((a ...) (and (equal? #t a) ...)))

(define (rnrs-exception->object proc)
  "procedure -> procedure
   wraps the given procedure so that when called and an exception occurs in it,
   the exception is catched and the object passed to raise is returned"
  (l a (guard (obj (#t obj)) (apply proc a))))

(define* (rnrs-exception->key proc #:optional return-object?)
  "procedure [boolean] -> symbol
   wraps the given procedure so that when called and an exception occurs in it,
   the exception is catched and if the exception object passed to raise is a symbol or list
   with a symbol as the first argument, the symbol is returned. otherwise the exception
   is re-raised or the object returned, depending on the boolean value of the second argument \"return-object?\""
  (l a
    (guard
      (obj
        (#t
          (if (symbol? obj) obj
            (if (and (list? obj) (not (null? obj)) (symbol? (first obj))) (first obj)
              (if return-object? obj (raise obj))))))
      (apply proc a))))

(define (guile-exception->key proc)
  "procedure -> procedure
   guard catches guile exceptions, but it seems impossible to get the key"
  (l a (catch #t (l () (apply proc a)) (l (key . a) key))))

(define (remove-keyword-associations a)
  "list -> list
   removes keyword associations from an argument list passed for example to lamdba*
   (3 #:a 1 2 #:b 4) -> (3 2)"
  (let loop ((rest a))
    (if (null? rest) rest
      (let (element (first rest))
        (if (keyword? element) (loop (list-tail rest 2)) (pair element (loop (tail rest))))))))

(define (search-env-path a)
  "(string ...) -> (string ...)
   search for any match of paths \"a\" in the directories in the PATH environment variable and result in the full path.
   similar to guiles %search-load-path but does not consider filename extensions"
  (let*
    ( (path-parsed (parse-path (getenv "PATH")))
      (search-path
        (l (a)
          (any (l (e) (let (path (string-append e "/" a)) (if (file-exists? path) path #f)))
            path-parsed))))
    (map search-path a)))

(define (search-env-path-one a) (first-or-false (search-env-path (list a))))

(define (call-at-interval proc interval)
  "procedure:{current-interval procedure:continue:{next-interval -> any} -> any} microseconds -> unspecified
   call proc after interval of length interval. proc receives the current interval and a procedure to
   continue, with the option to change the interval"
  (let next ((interval interval)) (usleep interval) (proc interval next)))

(define (call-at-interval-w-state proc interval . init)
  "procedure:{current-interval continue any ...} microseconds any ...
   like call-at-interval but accepting unlimited number of additional parameters
   that are passed to proc and the continue procedure."
  (letrec ((next (l (interval . state) (usleep interval) (apply proc interval next state))))
    (apply next interval init)))

(define*
  (call-at-approximated-interval proc min-interval max-interval #:optional (change-factor-slower 1.1) (change-factor-faster (/ 1 change-factor-slower)) . state)
  "{states ... -> integer} microseconds microseconds rational rational any ... ->
   like call-at-interval-w-state but without a continue procedure, automatically adjusting interval after each application of proc
   depending on the result of proc: -1 increases the interval, 1 decreases it and 0 does not change it."
  (apply call-at-interval-w-state
    (l (interval continue . state)
      (let ((change+state (apply proc state)))
        (apply continue
          (if (= 0 (first change+state)) interval
            (bound
              (inexact->exact
                (round
                  (* interval
                    (if (> (first change+state) 0) change-factor-faster change-factor-slower))))
              min-interval max-interval))
          (tail change+state))))
    min-interval state))

(define* (procedure->cached-procedure proc #:optional (key-f (l a a)))
  "procedure -> procedure procedure
   returns a new procedure with the same signature that calculates each result only once.
   calling the procedure again with the same arguments will lead to the cached result to be returned.
   the second returned procedure takes no arguments and clears the cache when called.
   example 1:
     (define my-calculate-cached (procedure->cached-procedure my-calculate))
   example 2:
     (let-values (((my-calculate-cached my-calculate-cached-reset) (procedure->cached-procedure my-calculate)))
       (my-calculate-cached-reset))"
  (let (cache (ht-make ht-hash-equal equal?))
    (values
      (l a
        (let* ((key (apply key-f a)) (cached-result (ht-ref cache key (q --not-in-cache))))
          (if (eq? (q --not-in-cache) cached-result)
            ((l (r) (ht-set! cache key r) r) (apply proc a)) cached-result)))
      (nullary (ht-clear! cache)))))

(define (procedure->temporarily-cached-procedure cache-duration proc)
  "integer:seconds procedure -> procedure
   like procedure->cached-procedure but the cache is emptied after cache-duration the next time the procedure is called"
  (let ((cache (ht-make ht-hash-equal equal?)) (last-time 0))
    (l a
      (let (time (current-time))
        (if (> (- time last-time) cache-duration)
          (begin (set! last-time time)
            ((l (result) (ht-set! cache a result) result) (apply proc a)))
          (let (cached-result (ht-ref cache a (q --not-in-cache)))
            (if (eq? (q --not-in-cache) cached-result)
              ((l (result) (ht-set! cache a result) result) (apply proc a)) cached-result)))))))

(define (program-path)
  "-> string
   return the full-path of the currently executed program file. uses the first argument of (program-arguments)"
  (let (part (first (program-arguments)))
    (if (string-null? part) part
      (if (eqv? #\/ (string-ref part 0)) part (string-append (getenv "PWD") "/" part)))))

(define (program-name)
  "-> string
   return the file-name of the currently executed program file. uses the first argument of (program-arguments)"
  (let (part (first (program-arguments)))
    (if (string-null? part) part (if (eqv? #\/ (string-ref part 0)) (basename part) part))))

(define (procedure-append . proc)
  "procedure ... -> procedure:{any ... -> (any ...)}
   creates a new procedure that applies each appended procedure with given arguments
   and returns the results of each call in a list"
  (l a (map (l (b) (apply b a)) proc)))

(define (procedure-append* . proc)
  "procedure ... -> procedure:{any ... -> unspecified}
   like procedure-append but procedure call results must be lists
   which are appended"
  (l a (append-map (l (b) (apply b a)) proc)))

(define (procedure-append-ignore-result . proc)
  "procedure ... -> procedure:{any ... -> unspecified}
   like procedure-append but does not collect results and returns unspecified"
  (l a (each (l (b) (apply b a)) proc)))

(define* (cli-option name #:optional value)
  "creates a string for one generic command-line option in the gnu format -{single-character} or --{word} or --{word}=.
   optionally with values.
   \"name\"can be a:
   character: short option
   string: long option
   value can be anything, non-strings will be converted to strings in \"display\" format"
  (if value
    (if (char? name) (string-append (string #\- name #\space) (any->string value))
      (string-append "--" name "=" (any->string value)))
    (if (char? name) (string #\- name) (string-append "--" name))))

(define (cli-option-join options)
  "((name value ...)/string ...) -> (string ...)
   ((\"a\" 3)) -> -a 3
   ((\"abc\" 3)) -> --abc 3
   creates a command-line options string, automatically appending - or -- to option names.
   - pairs with prefixes that are characters or single char strings become single char options
   - pairs with prefixes that are multi char strings become multi char options
   - the tail of pairs is string-joined with spaces and used as the value for the option
   - strings become keyless arguments"
  (string-join
    (fold
      (l (e r)
        (if (string? e) (pair e r)
          (if (and (list? e) (not (null? e)))
            (pair
              (let*
                ( (name (first e)) (name (if (char? name) (string name) name)) (value (tail e))
                  (value
                    (if (null? value) ""
                      (string-append " " (if (string? value) value (string-join value " "))))))
                (string-append (if (= (string-length name) 1) "-" "--") name value))
              r)
            r)))
      (list) options)
    " "))

(define socket-bind bind)

(define* (each-integer count proc #:optional (start 0))
  "integer procedure:{integer ->} ->
   call proc \"count\" times"
  (let (end (+ start count)) (let loop ((n start)) (if (< n end) (begin (proc n) (loop (+ 1 n)))))))

(define (pass proc obj)
  "procedure any -> any
   apply proc with obj, return obj.
   example
   (+ 2 (pass write (+ 1 3)))
   writes 4 to standard output
   results in 6"
  (proc obj) obj)

(define (char-set->vector a) (list->vector (char-set->list a)))

(define (pass-predicate-and-if predicates subject consequent alternative)
  "(procedure ...) any procedure:{any:subject -> any} procedure:{any:subject -> any}"
  (if (every (l (a) (a subject)) (any->list predicates)) (consequent subject) (alternative subject)))

(define (pass-predicate-or-if predicates subject consequent alternative)
  "any/(procedure:{any:subject -> any} ...) any procedure:{any:subject -> any} procedure:{any:subject -> any}"
  (if (any (l (a) (a subject)) (any->list predicates)) (consequent subject) (consequent subject)))

(define (predicate-and predicates . subjects)
  "any/(procedure:{any:subject -> any} ...) any ... -> boolean
   true if every predicate gives true for every subject, false otherwise"
  (every (l (a) (every (l (b) (a b)) subjects)) (any->list predicates)))

(define (predicate-or predicates . subjects)
  "any/(procedure:{any:subject -> any} ...) any ... -> boolean
   true if every predicate gives true for every subject, false otherwise"
  (any (l (a) (any (l (b) (a b)) subjects)) (any->list predicates)))

(define (boolean->integer a) (if a 1 0))

(define (repeat f repeat-count state)
  "procedure:{-> any} integer false/repeat-state:(any:last-result integer:count) -> (any:result integer:count)
   f is called without arguments and generates new values that are returned again on subsequent calls for repeat-count times.
   returned is a new repeat state which is a list with as the first element the value returned by f.
   if state is false then a new state object will be created and returned"
  (apply (l (previous count) (if (< count repeat-count) (list previous (+ 1 count)) (list (f) 1)))
    (or state (list (f) 0))))

(define* (random-discrete-f probabilities #:optional (state *random-state*))
  "(real ...) [random-state] -> procedure:{-> real}
   return a function that when called returns an index of a
   value in probabilities.
   each index will be returned with a probability given by the value at the index.
   each value is a fraction of the sum of probabilities.
   for example, if the values sum to 100, each entry in probabilities is a percentage.
   this allows for custom discrete random probability distributions.
   example usage:
   (define random* (random-discrete-f (list 10 70 20)))
   (define samples (list (random*) (random*)))"
  (let* ((cuprob (apply cusum probabilities)) (sum (last cuprob)))
    (nullary
      (let (deviate (random sum state))
        (let loop ((a 0) (b cuprob))
          (if (null? b) a (if (< deviate (first b)) a (loop (+ 1 a) (tail b)))))))))

(define (random-between min max) (+ min (random (- max min))))

(define* (random-boolean #:optional percentage)
  "[integer] -> boolean
   percentage gives the probability of true results"
  (if percentage (> percentage (random 100)) (= (random 2) 0)))

(define (random-bytevector size) "integer -> bytevector"
  (let (result (make-bytevector size))
    (each-integer size (l (index) (bytevector-u8-set! result index (random 256)))) result))

(define (random-ascii-string len)
  "integer -> string
   results in a string of randomly chosen ascii characters excluding control characters"
  (let loop ((n 0) (r (list)))
    (if (< n len) (loop (+ 1 n) (cons (integer->char (+ 32 (random 94))) r)) (list->string r))))

(define char-set-vector:designated (list->vector (char-set->list char-set:designated)))

(define*
  (random-chars list-length #:optional (char-set char-set-vector:designated) (state *random-state*))
  "integer string/vector random-state -> (character ...)
   creates a list of random chars from a set of characters.
   the default set of characters includes all the code points to which unicode has assigned a character or other meaning"
  (let*
    ( (a (if (string? char-set) (string->utf8 char-set) char-set))
      (a-last-index (- (vector-length a) 1)))
    (map-integers list-length (l (n) (vector-ref a (random a-last-index state))))))

(define*
  (random-string #:optional (len (random 255)) (char-set char-set-vector:designated)
    (state *random-state*))
  "[integer string/vector] -> string
   the default set of characters includes all the code points to which unicode has assigned a character or other meaning"
  (list->string (random-chars len char-set state)))
