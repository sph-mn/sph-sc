(define-module (sph lang sc sph list))
(use-modules (ice-9 match) (sph lang sc sph) (srfi srfi-31) (srfi srfi-1) ((rnrs sorting) #:select (list-sort)))

(re-export list-sort)

(export any->list compact
  complement complement-both
  consecutive contains-all?
  contains-some? contains?
  containsq? containsv-some?
  containsv? convolve
  count-value count-value-with-limit
  count-with-limit define-list
  delete-duplicates-sorted difference
  difference-and-intersection difference-and-intersection-p
  difference-p drop*
  duplicates each-first-middle-last
  each-in-index-range each-slice
  each-with-index every-fold
  every-map every-or-index
  false-if-null filter-append-map
  filter-produce first-intersection
  first-intersection-p first-or-false
  first-or-null flat?
  flatten fold*
  fold-c* fold-integers
  fold-multiple fold-multiple-c
  fold-multiple-right fold-right*
  fold-segments fold-slice
  fold-span fold-unless
  fold-unless-check-init fold-until
  fold-with-buffer group-consecutive
  group-split-at-matches improper-list-split-at-last
  insert-second interleave
  intersection intersection-p
  iterate-three iterate-three-stop-end
  let*-list list-bind
  list-deselect list-distribute
  list-distribute-sorted list-index-value
  list-indices list-logical-condition?
  list-logical-contains? list-logical-match
  list-page list-prefix?
  list-replace-last list-replace-last-n
  list-select list-set-equal?
  list-set-eqv? list-sort-by-list
  list-sort-with-accessor list-suffix?
  map-apply map-c
  map-consecutive map-first
  map-fold map-integers
  map-last-n map-map
  map-one map-segments
  map-selected map-slice
  map-span map-unless
  map-with-index pair->list
  pair-bind pair-fold-multiple
  pair-map pair-reverse
  pattern-match-min-length produce
  produce-controlled produce-unless
  replace replace-at-once
  replace-value simplify
  simplify-list sph-list-description
  splice splice-last-list
  split-at-last split-at-value
  split-by-pattern tail-or-null
  take* take-right*
  true->list union list-set-difference list-set-add list-tail-ref list-set-union list-set-subset?
  group group-recursively
  list-ref-random list-ref-randomize-cycle
  randomize sph-list-other-description
  alist alist-ref alist-set list->alist alist-prepend)

(define list-set-difference lset-difference)
(define list-set-add lset-adjoin)
(define list-tail-ref list-tail)
(define list-set-union lset-union)
(define list-set-subset? lset<=)

(define sph-list-description
  "list helpers.
   currently also contains bindings for non-list pairs.
   # highlights
   list-logical-match: match values by (and (or (and 1 2 3) 4)) conditions
   list-sort-with-accessor: sort any kind of element as long as the value to sort by can be accessed by a procedure
   split-by-pattern: rudimentary ellipsis pattern matching. a more powerful version can currently be found in sph-sc
   compact: remove false values from a list
   flatten: merge sublists
   map-slice: map over each overlapping segment of length
   map-segments: map over each non-overlapping segment of length
   fold*: fold with multiple state values
   group: build an association list from a list with a custom predicate
   group-recursively
   # syntax
   ## let*-list
   like let*, but variable names enclosed in round brackets bind list elements.
   binding is done like lambda and apply.
   examples
     (let*-list ((a 1) ((b c) (list 2 3))) (and (= 1 a) (= 2 b) (= 3 c)))
     (let*-list (((a b . c) (list 1 2 3 4))) #t)")

(define-syntax-rule (identity-if test else ...) ((lambda (r) (if r r (begin else ...))) test))

(define-syntax-rule (list-bind a lambda-formals body ...)
  "bind elements of list \"a\" to \"lambda-formals\"" (apply (lambda lambda-formals body ...) a))

(define-syntax-rules let*-list
  ( ( ( ( (name ...) a) rest ...) body ...)
    (apply (lambda (name ...) (let*-list (rest ...) body ...)) a))
  ((((name a) rest ...) body ...) ((lambda (name) (let*-list (rest ...) body ...)) a))
  ((() body ...) (begin body ...)))

(define-syntax-rule (define-list name a ...) (define name (list a ...)))

(define-syntax-rule (pair-bind a (b c) body ...)
  "bind the first and second value of \"a\" to \"b\" and \"c\" respectively.
   ideally, maybe, lambda/apply should support (apply (lambda (a . b)) (pair 1 2))"
  ((lambda (b c) body ...) (first a) (tail a)))

(define (drop* count a)
  "like srfi-1 drop but with reversed argument order (like stream-drop from srfi-41) and
   returns null if list contains less elements than count instead of raising an exception"
  (if (<= (length a) count) (list) (drop a count)))

(define (take* count a)
  "like srfi-1 take but with reversed argument order (like stream-take from srfi-41) and
   returns null if list contains less elements than count instead of raising an exception"
  (if (<= (length a) count) a (take a count)))

(define (take-right* count a)
  "like srfi-1 take-right but with reversed argument order (like stream-take from srfi-41) and
   returns null if list contains less elements than count instead of raising an exception"
  (if (<= (length a) count) a (take-right a count)))

(define (list-page a entry-count number lookahead c)
  "list integer integer integer procedure:{list boolean:last-page? -> any} -> any
   pass a list of \"entry-count\" elements at an offset of (* number entry-count),
   eventually including \"lookahead\" number of elements if they are the last elements
   and a boolean indicating if it is the last page to continuation procedure \"c\""
  (let*
    ( (offset (* (- number 1) entry-count)) (rest (if (< 1 number) (drop* offset a) a))
      (lookahead-entries (take* lookahead (drop* entry-count rest))) (page (take* entry-count rest)))
    (if (= lookahead (length lookahead-entries)) (c page #f) (c (append page lookahead-entries) #t))))

(define (flatten a)
  "list -> (non-list ...)
   replace sublists with their content, resulting in a list that does not contain lists"
  (fold-right (l (e r) (if (list? e) (append (flatten e) r) (pair e r))) (list) a))

(define (append-map-unless f stop? default . a)
  "procedure:{any:list-element ... -> any} procedure:{any -> boolean} any list ... -> list/false
   map unless \"stop?\" is true for a map result. result in \"default\" if \"stop?\" is true"
  (if (any null? a) (list)
    (let loop ((rest (map tail a)) (b (apply f (map first a))) (init (list)))
      (if (stop? b) default
        (if (any null? rest) (append init b)
          (loop (map tail rest) (apply f (map first rest)) (append init b)))))))

(define (non-empty-list? a)
  "any -> boolean
   true if argument is a list with at least one element"
  (and (list? a) (not (null? a))))

(define (every-or-index f . a)
  "procedure:{any ... -> boolean} list ... -> true/integer
   true if \"f\" is true for all elements, otherwise the index of the element for which \"f\" was false"
  (let (r (apply list-index (negate (l e (apply f e))) a)) (if (boolean? r) (not r) r)))

(define (fold-every f init . a)
  "{any any -> any} any list ... -> any
   like fold, but every result must be a \"true\" value, otherwise the result is false"
  (if init (if (null? a) init (fold-every f (f (map first a) init) (map tail a))) init))

(define (any->list a)
  "any -> list
   wraps a non-list argument in a list"
  (if (list? a) a (list a)))

(define (map-first f a)
  "procedure list -> list
   call \"f\" for the first element of list and replace the first element in the list with the result of \"f\".
   replace-first"
  (pair (f (first a)) (tail a)))

(define (replace-at-once match? f a)
  "procedure:{any -> boolean} procedure:{list:matched-elements -> list:replacements} list:source -> list
   all elements matching \"match?\" are collected in a list and passed to \"f\".
   the result of \"f\" is then used to replace the matched elements in source in order"
  (reverse
    (first
      (let (a-extended (map (l (e) (pair (match? e) e)) a))
        (fold-multiple
          (l (e r replacements)
            (if (first e) (list (pair (first replacements) r) (tail replacements))
              (list (pair (tail e) r) replacements)))
          a-extended (list) (f (filter-map (l (e) (if (first e) (tail e) #f)) a-extended)))))))

(define (contains-all? a values)
  "list ... -> boolean
   test if argument \"a\" contains all of the given values"
  (every (l (b) (contains? a b)) values))

(define (contains-some? a values)
  "list ... -> boolean
   test if argument \"a\" contains any of the given values"
  (any (l (b) (contains? a b)) values))

(define (containsv-some? a values)
  "list list -> boolean
   test if argument \"a\" contains any of the given values"
  (any (l (b) (containsv? a b)) values))

(define* (contains? a value #:optional (member member))
  "list any [procedure:{any list -> boolean/any}] -> boolean
   return a boolean indicating if list \"a\" contains \"value\""
  (if (member value a) #t #f))

(define (containsq? a value)
  "true if list contains value.
   comparison with eq?"
  (if (memq value a) #t #f))

(define (containsv? a value)
  "true if list contains value.
   comparison with eqv?"
  (if (memv value a) #t #f))

(define* (count-value value a #:optional (equal? equal?))
  "any list -> integer
   count occurences of \"value\" in list"
  (fold (l (a r) (if (equal? value a) (+ 1 r) r)) 0 a))

(define* (count-value-with-limit value a #:optional (count-limit (inf)) (member member))
  "any list [integer procedure:{any list -> boolean/any}] -> integer
   like count-value but with an optional parameter for a count at which to stop counting"
  (let loop ((rest (member value a)) (count 0))
    (if (pair? rest)
      (let (count (+ count 1))
        (if (= count-limit count) count (loop (member value (tail rest)) count)))
      count)))

(define* (count-with-limit pred limit . a)
  "procedure integer list ... -> integer
   like \"count\" but with an optional parameter for a count at which to stop counting"
  (let loop ((rest a) (count 0))
    (if (any null? rest) count
      (if (apply pred (map first rest))
        (let (count (+ 1 count)) (if (= count limit) count (loop (map tail rest) count)))
        (loop (map tail rest))))))

(define (produce-one f a b) "procedure any list -> list" (map (l (b) (f a b)) b))

(define (convolve a b)
  "list list -> list
   returns the discrete, linear convolution of two one-dimensional sequences.
   the length of the result will be (a-length + b-length - 1).
   example use case: modelling the effect of a linear time-invariant system on a signal"
  "algorithm: sum the tails of productions for each element of \"a\""
  (let loop ((products (produce-one * (first a) b)) (a (tail a)))
    (if (null? a) products
      (pair (first products)
        (loop (map + (append (tail products) (list 0)) (produce-one * (first a) b)) (tail a))))))

(define (complement-both a b)
  "list list -> (list list)
   delete elements in both lists that are included in both lists"
  (list (complement a b) (complement b a)))

(define (complement . lists)
  "list ... -> list
   delete elements from the first list that are included in the other lists"
  (apply lset-difference equal? lists))

(define (union . a) (delete-duplicates (apply append a)))

(define (duplicates a) "get a list of distinct values that occur more than once in the same list"
  (delete-duplicates (filter-map (l (b) (and (< 1 (count-value b a)) b)) a)))

(define* (delete-duplicates-sorted a #:optional (equal-f equal?) (preserve-order #t))
  "list [procedure:{any any -> boolean} boolean] -> list
   delete duplicates from a sorted list using a more efficient algorithm than for unsorted lists"
  (if (or (null? a) (null? (tail a))) a
    ( (if preserve-order reverse (l (i) i))
      (let loop ((e-1 (first a)) (rest (tail a)) (r (list (first a))))
        (if (null? rest) r
          (let ((e-2 (first rest))) (loop e-2 (tail rest) (if (equal-f e-1 e-2) r (pair e-2 r)))))))))

(define (each-slice f slice-length a)
  "procedure:{list ->} integer list ->
   apply f to each slice of slice-length elements from list."
  (let loop ((rest a) (cur-slice (list)) (cur-length 0))
    (if (null? rest) (if (not (null? cur-slice)) (f cur-slice))
      (if (= slice-length cur-length)
        (begin (f cur-slice) (loop (tail rest) (list (first rest)) 1))
        (loop (tail rest) (pair (first rest) cur-slice) (+ 1 cur-length))))))

(define (each-with-index f . a)
  "procedure:{index element ... ->} list ... ->
   apply f to each element and its index in list a"
  (let loop ((rest a) (index 0))
    (if (not (any null? rest))
      (begin (apply f index (map first rest)) (loop (map tail rest) (+ index 1))))))

(define (each-in-index-range f start end . a)
  "procedure integer integer list ... ->
   untested.
   call f only for elements in index range between \"start\" and \"end\" inclusively"
  (let loop ((rest a) (index (if (< end 0) (+ end (length (first a))) 0)))
    (if (<= index end)
      (begin (if (>= index start) (apply f (map first rest))) (loop (map tail rest))))))

(define (each-first-middle-last first-f middle-f last-f . a)
  "procedure procedure procedure list ... ->
   untested.
   call \"first-f\" for the first element,
   call \"last-f\" for the last element,
   call \"middle-f\" for a list of all elements inbetween"
  (apply first-f (map first a))
  (let loop ((rest (map tail a)) (count (- (length (first a)) 1)))
    (if (= 0 count) (apply last-f (map first rest))
      (begin (middle-f (map first rest)) (loop (map tail rest) (- count 1))))))

(define (every-map f . a)
  "procedure:{any -> any} list ... -> list/false
   like map but results in false if any result of f is not a true value"
  (apply map-unless f not #f a))

(define (every-fold f state . a)
  "procedure:{any:state any:element ... -> any:next-state} list:elements ... -> any:state/false
   like fold but results in false if any result of f is not a true value"
  (apply fold-unless f not #f state a))

(define (compact a)
  "list -> list
   keep only true elements in list. removes all boolean false values"
  (filter identity a))

(define (filter-append-map f . lists) "apply filter-map and then apply append on the result"
  (apply append (apply filter-map f lists)))

(define (difference-p equal-f . lists)
  "{any any -> boolean} list ... -> list
   like"
  difference " but the predicate for comparing list elements can be specified"
  (let (every* (l (a b) (every (l (e-2) (any (l (e-3) (equal-f a e-3)) e-2)) b)))
    (iterate-three
      (l (head e rest . r)
        (append (filter (l (e) (not (and (every* e rest) (every* e head)))) e) r))
      lists)))

(define (difference . lists)
  "list ... -> list
   result in a list of elements not included in all given lists"
  (apply difference-p equal? lists))

(define (difference-and-intersection-p equal-f . lists)
  "{any any -> boolean} list ... -> (list:difference list:intersection)
   like difference+intersection but the predicate for comparing list elements can be specified"
  (let (every* (l (a b) (every (l (e-2) (any (l (e-3) (equal-f a e-3)) e-2)) b)))
    (iterate-three
      (l (head e rest d i)
        (fold-multiple
          (l (e d i)
            (if (and (every* e rest) (every* e head)) (list d (pair e i)) (list (pair e d) i)))
          e d i))
      lists (list) (list))))

(define (difference-and-intersection . lists)
  "list ... -> (list list)
   results in a list with two elements, one being the symmetric-difference between the given lists and one being the intersection.
   that means one list of all elements that are included in all lists, and one list of elements that are not included in all lists.
   it does both calculations in one step saving resources compared to making them in separate steps."
  (apply difference-and-intersection-p equal? lists))

(define (intersection-p equal-f . rest)
  "procedure:{any any -> boolean} list ... -> list
   like \"intersection\" but the predicate for comparing the list elements can be specified"
  (if (any null? rest) (list)
    (filter (l (e) (every (l (e-2) (any (l (e-3) (equal-f e e-3)) e-2)) (tail rest))) (first rest))))

(define (intersection . lists)
  "list ... -> list
   result in a list of all elements which are contained in all given lists"
  (apply intersection-p equal? lists))

(define (filter-produce f . a)
  "procedure list ...
   apply \"f\" to each ordered combination of elements from lists, cartesian product, and return true results in a list.
   supports multiple lists and treats non-list arguments as the single element of a list.
   example:
     (produce f (1 2) (4 5) 6)
   is equivalent to
     (list (f 1 4 6) (f 1 5 6) (f 2 4 6) (f 2 5 6))"
  (let loop ((rest (map any->list a)) (args (list)))
    (if (null? rest) (apply f args)
      (let (tail-rest (tail rest))
        ( (if (null? tail-rest) filter-map filter-append-map) (l e (loop tail-rest (append args e)))
          (first rest))))))

(define (first-intersection-p equal-f a b)
  "{any any -> boolean} list list -> any
   like first-intersection but the procedure for comparing elements can be specified"
  (find (l (b) (any (l (a) (equal-f a b)) a)) b))

(define (first-intersection a b)
  "list list -> any
   give the first found element that is included in both lists"
  (first-intersection-p equal? a b))

(define (first-or-false a)
  "list -> any/false
   give the first element of a list if it is not null, otherwise false"
  (if (null? a) #f (first a)))

(define (false-if-null a) (if (null? a) #f a))

(define (first-or-null a)
  "results in the first element of a list if it is not null, otherwise null"
  (if (null? a) a (first a)))

(define (flat? a)
  "list -> boolean
   true if the list does not contain a list"
  (if (null? a) #t (if (list? (first a)) #f (flat? (tail a)))))

(define (fold-span filter-f f a)
  "procedure:{any -> any/false} procedure:{list -> any} list -> any
   fold over each list of elements that consecutively matched filter-f (utilising the \"span\" procedure)"
  (let loop ((rest a) (r (list)))
    (if (null? rest) (reverse r)
      (apply-values
        (l (consecutive rest)
          (if (null? consecutive) (loop (tail rest) (pair (first rest) r))
            (loop rest (f consecutive r))))
        (span filter-f rest)))))

(define (fold-multiple f a . custom-state-values)
  "procedure:{any:list-element any:state-value ... -> (any:state-value)} list any:state-value ... -> list:state-values
   like fold but with multiple state values. the state values are updated by returning a list from a call to \"f\".
   apply \"f\" to each element of \"a\" and the state-value elements that were given to
   fold-multiple or subsequently the updated state-values from the previous call to \"f\""
  (if (null? a) custom-state-values
    (apply fold-multiple f (tail a) (apply f (first a) custom-state-values))))

(define (fold-multiple-c f a . custom-state-values)
  "procedure:{any:element procedure:continue:{list:next-pair any:state-value ...} any:state-value ... -> any} list any:state-value ... -> list"
  (if (null? a) custom-state-values
    (apply f (first a)
      (l custom-state-values (apply fold-multiple-c f (tail a) custom-state-values))
      custom-state-values)))

(define (fold-multiple-right f a . r)
  "procedure list any ... -> any
   like fold-multiple but works through the list elements from last to first"
  (if (null? a) r (apply f (first a) (apply fold-multiple-right f (tail a) r))))

(define fold* fold-multiple)
(define fold-c* fold-multiple)
(define fold-right* fold-multiple-right)

(define (fold-segments size f init a)
  "integer {any:state element ... -> any:state}  any:state list -> any
   fold over each overlapping segment with length \"size\".
   example:
   (fold-segments f 2 #t (list 4 5 6 7))"
  (let loop ((rest a) (buf (list)) (r init) (count size))
    (if (null? rest) (if (null? buf) r (apply f r buf))
      (if (< count 1) (loop (tail rest) (append (tail buf) (list (first rest))) (apply f r buf) 0)
        (loop (tail rest) (append buf (list (first rest))) r (- count 1))))))

(define (fold-unless f stop? default init . a)
  "{any ... -> any} {any -> boolean/any} any any list ... -> any
   like fold, but returns \"default\" if \"stop?\" is true"
  (if (any null? a) init
    (apply fold-unless-check-init f
      stop? default (apply f (append (map first a) (list init))) (map tail a))))

(define (fold-unless-check-init f stop? default init . a)
  (if (stop? init) default
    (if (any null? a) init
      (apply fold-unless-check-init f
        stop? default (apply f (append (map first a) (list init))) (map tail a)))))

(define (fold-until f init stop? a)
  "procedure any procedure:{any -> boolean} list -> any
   end folding if \"stop?\" is true for a result and return the result"
  (if (or (null? a) (stop? init)) init (fold-until f (f (first a) init) stop? (tail a))))

(define (group-consecutive filter-f a)
  "{any -> boolean} list -> list
   wrap multiple elements that consecutively match \"filter-f\" in a list"
  (map-consecutive filter-f (l a a) a))

(define (improper-list-split-at-last a)
  "pair:improper-list -> (list any:non-pair)
   (1 2 . 3) -> ((1 2) 3)"
  (let loop ((rest a) (r (list)))
    (if (pair? rest) (loop (tail rest) (pair (first rest) r)) (pair (reverse r) rest))))

(define (interleave a value)
  "list any -> list
   inserts value in front of each element in \"a\" except the first element.
   example: (interleave (list 1 2 3) 4) -> (1 4 2 4 3)"
  (if (null? a) a (reverse (fold (l (e r) (pairs e value r)) (list (first a)) (tail a)))))

(define iterate-three
  (letrec
    ( (loop
        (l (f prev current next . states)
          (if (null? next) (apply f prev current next states)
            (apply loop f
              (pair current prev) (first next) (tail next) (apply f prev current next states))))))
    (l (f a . states)
      "procedure:{list:prev any:current list:next any:state ... -> any:state ...} list any:state-init ... -> list:state
       calls \"f\" for each list element, previous list elements and following list elements.
       multiple custom values can be updated each call with the result of \"f\" which must be a list"
      (apply loop f (list) (first a) (tail a) states))))

(define iterate-three-stop-end
  (letrec
    ( (loop
        (l (stop? end map-f r current next . states)
          (if (or (null? next) (and stop? (apply stop? r current next states)))
            (apply (or end map-f) r current next states)
            (apply loop stop?
              end map-f (pair current r) (first next) (tail next) (apply map-f r current next states))))))
    (l (stop? end map-f a . states)
      "{list any list any ... -> boolean} {list any list any ... -> list:state-values}:after-stop? {list any list any ... -> list:state-values} list any ... -> any
       like \"iterate-three\" but takes two additional procedures - one for stopping the iteration
       after a \"map-f\" result, and one that is called for the last element or when \"stop?\" is true"
      (loop stop? end map-f (list) (first a) (tail a)))))

(define (list-distribute-sorted a indices default)
  "like list-distribute but faster. works only correctly for indices lists that are sorted ascending"
  (let (last-index (last indices))
    (let loop ((index 0) (indices indices) (rest a) (r (list)))
      (if (= index last-index) (reverse (pair (first rest) r))
        (if (= index (first indices))
          (loop (+ 1 index) (tail indices) (tail rest) (pair (first rest) r))
          (loop (+ 1 index) indices rest (pair default r)))))))

(define (list-distribute a indices default)
  "list (integer ...) any -> list
   creates a new list with values from list a at positions indices. the value for \"no-element\" is set at indices
   not included in the list indices. the length of indices must equal the length of a, and indices should not have duplicates."
  (let (indices (list-sort (l (a b) (< (first a) (first b))) (map cons indices a)))
    (list-distribute-sorted (map tail indices) (map first indices) default)))

(define* (list-index-value a value #:optional (equal-f equal?)) "get the index of value in list"
  (list-index (l (a) (equal-f a value)) a))

(define (list-indices f a)
  "procedure:{any -> boolean} list -> (integer ...)
   create a list of all indices for which f results in true"
  (let loop ((rest a) (index 0) (r (list)))
    (if (null? rest) r (loop (tail rest) (+ 1 index) (if (f (first rest)) (pair index r) r)))))

(define (list-prefix? a prefix)
  "list list -> boolean
   true if the given \"prefix\" elements exist in order at the beginning of list.
   examples:
   (list-prefix? (list 3 2 4) (list 3 1)) -> #f
   (list-prefix? (list 3 2 4) (list 3 2)) -> #t"
  (let (length-prefix (length prefix))
    (if (< (length a) length-prefix) #f (equal? (take a length-prefix) prefix))))

(define (list-suffix? a suffix)
  "list list -> boolean
   true if the given \"suffix\" elements exist in order at the end of list.
   see also \"list-prefix?\""
  (let (length-suffix (length suffix))
    (if (< (length a) length-suffix) #f (equal? (take-right a length-suffix) suffix))))

(define (split-at-last a)
  "list -> (list list)
   get a list with the list of the initial elements and a list with the last element"
  (if (> 2 (length a)) (list a (list))
    (let (a-reverse (reverse a)) (list (reverse (tail a-reverse)) (list (first a-reverse))))))

(define (map-last-n n b f)
  "procedure:{any ... -> any/(any ...)} list -> list
   call f to replace the last n elements in list b.
   if the result of f is a list it is spliced so that the
   elements can be replaced with multiple elements"
  (if (null? b) b
    (apply-values (l (last-n rest) (append (reverse rest) (any->list (apply f (reverse last-n)))))
      (split-at (reverse b) n))))

(define (list-replace-last a replacement)
  "list any/procedure:{any -> any} -> list
   replace the last element in a list"
  (list-replace-last-n 1 a replacement))

(define (list-replace-last-n n a replacement)
  "list integer any/procedure:{any ... -> any/list} -> list"
  (map-last-n n a (l a (list replacement))))

(define (list-select a indices)
  "list (integer ...) -> list
   return a new list consisting of values at indices"
  (map (l (b) (list-ref a b)) indices))

(define (list-deselect a indices)
  "list (integer ...) -> list
   return a new, eventually smaller, list consisting of values not at specified indices"
  (let loop ((rest a) (index 0))
    (if (null? rest) rest
      (if (containsv? indices index) (loop (tail rest) (+ 1 index))
        (pair (first rest) (loop (tail rest) (+ 1 index)))))))

(define (list-set-equal? . a)
  "list ... -> boolean
   true if all elements of the given lists appear in all others.
   uses \"equal?\" for element equality comparison"
  (apply lset= equal? a))

(define (list-set-eqv? . a)
  "list ... -> boolean
   like \"list-set-equal?\" but uses \"eqv?\" for element equality comparison"
  (apply lset= eqv? a))

(define (list-set-eq? . a)
  "list ... -> boolean
   like \"list-set-equal?\" but uses \"eq?\" for element equality comparison"
  (apply lset= eq? a))

(define (list-logical-contains? a condition)
  "list list -> boolean
   test for value inclusion with a condition list like ([or/and/not] value/condition ...).
   example:
   (list-logical-contains? (list 1 2 3) (quote (and 2 3 (or 4 1 5) (not 8)))) -> #t"
  (list-logical-match (l (b) (contains? a b)) condition))

(define (list-logical-match match-one? condition)
  "procedure:{any -> boolean} list -> false/any:last-sub-condition-result
   match a logical condition that is a possibly nested list with and/or/not symbol prefixes.
   match-one? is called for each element in condition that is not a condition prefix.
   returns false early if a required part of the condition does not match.
   condition: ([symbol:and/or/not] any/condition ...)
   example
     (list-logical-match (l (b) (contains? somelist b)) (q (and 1 2 (or (and 3 4) (and 5 6)))))"
  (letrec
    ( (match
        (let (match-one? (l (a) ((if (list-logical-condition? a) match match-one?) a)))
          (l (a)
            (if (null? a) #f
              (case (first a)
                ((or) (any match-one? (tail a)))
                ((and) (every match-one? (tail a)))
                ((not) (not (any match-one? (tail a))))
                (else (any match-one? (list a)))))))))
    (match condition)))

(define (list-logical-condition? a)
  "any -> boolean
   true if \"a\" is a list-logical condition"
  (if (list? a) (if (null? a) #f (case (first a) ((or and not) #t) (else #f))) #f))

(define* (list-sort-by-list order a #:optional (accessor identity))
  "list list -> list
   sort a list so the elements correspond to the order of elements in list \"order\".
   elements not contained in \"order\" are moved to the end of the result list.
   examples:
   (list-sort-by-list (list 3 2 4) (list 4 2 3)) -> (3 2 4)
   (list-sort-by-list (list 3 2 4) (list 4 5 2 3)) -> (3 2 4 5)"
  (let (a-len (length a))
    (list-sort
      (l (a b)
        (< (identity-if (list-index-value order (accessor a)) a-len)
          (identity-if (list-index-value order (accessor b)) a-len)))
      a)))

(define (list-sort-with-accessor less? accessor a)
  "procedure:{any any -> boolean} procedure:{any:list-element -> any} list -> list
   sort list by calling accessor for each argument before comparison. only the order of elements changes, the individual elements are not changed"
  (list-sort (l (a b) (less? (accessor a) (accessor b))) a))

(define (map-selected select? f . a)
  "procedure procedure list ... -> list
   apply f only to elements for which \"select?\" is true. unmatched items are included in the result list.
   if multiple lists are given, it works like \"map\" except that the elements from the multiple lists for one call that are not selected are saved as a list.
   map-some/map-only"
  (apply map (l e (if (apply select? e) (apply f e) (if (null? (tail e)) (first e) e))) a))

(define (map-apply f . a)
  "procedure:{any ... -> any} (list ...) ... -> list
   like map but the procedure is applied with elements of \"a\" as arguments.
   instead of calling f like (f (list 1 2)) like \"map\" would do, f is called like (f 1 2)
   example
     (map-apply f (list (list 1 2) (list 3 4)))"
  (apply map (l a (apply f (apply append a))) a))

(define (map-map f . a)
  "procedure (list ...) ... -> list
   given a list of lists, maps over the elements of lists.
   like (map (l (a) (map f a) a))"
  (apply map (l a (apply map f a)) a))

(define (map-c f . lists)
  "procedure:{procedure:{any:new-element -> any}:continue any:element ... -> any:last-result} list ... -> list
   map over list with a procedure that when called with the current map result continues the mapping.
   if the procedure is not called, the result of the current call will become the tail of the result list.
   maps only the length of the shortest list if multiple lists are given
   example
     (map-c (l (c a) (if (> 3 a) (c (+ 1 a)) (list))) (list 1 2 3 4 5))
     ->
     (2 3 4)"
  (let loop ((rest lists) (len (apply min (map length lists))))
    (if (zero? len) (list)
      (apply f (l (result) (pair result (loop (map tail rest) (- len 1)))) (map first rest)))))

(define (map-one predicate f a)
  "{any -> any}:predicate {any:element -> any} list -> list
   apply f only to the first element that matches predicate.
   all elements that do not match are mapped with the \"identity\" function"
  (let loop ((rest a) (r (list)))
    (if (null? rest) r
      (let (e (first rest))
        (if (predicate e) (append (reverse (pair (f e) r)) (tail rest))
          (loop (tail rest) (pair e r)))))))

(define (map-segments size f a)
  "integer procedure:{any ... -> any} list -> list
   map over each overlapping segment of length len.
   each segment is one step apart.
   example: for (1 2 3 4) size 2 maps (1 2) (2 3) (3 4)"
  (fold-segments size (l (result . a) (append result (list (apply f a)))) (list) a))

(define (map-slice slice-length f a)
  "integer procedure:{any ... -> any} list -> list
   call \"f\" with each \"slice-length\" number of consecutive elements of \"a\""
  (let loop ((rest a) (slice (list)) (slice-ele-length 0) (r (list)))
    (if (null? rest) (reverse (if (null? slice) r (pair (apply f (reverse slice)) r)))
      (if (= slice-length slice-ele-length)
        (loop (tail rest) (list (first rest)) 1 (pair (apply f (reverse slice)) r))
        (loop (tail rest) (pair (first rest) slice) (+ 1 slice-ele-length) r)))))

(define (fold-slice slice-length f init a)
  "integer procedure:{any:state any:element ... -> any} any list -> any:state
   call f with each slice-length number of consecutive elements of a"
  (let loop ((rest a) (slice (list)) (slice-buffer-length 0) (r init))
    (if (null? rest) (reverse (if (null? slice) r (apply f r (reverse slice))))
      (if (= slice-length slice-buffer-length)
        (loop (tail rest) (list (first rest)) 1 (apply f r (reverse slice)))
        (loop (tail rest) (pair (first rest) slice) (+ 1 slice-buffer-length) r)))))

(define (map-consecutive filter-f f a)
  "{any -> boolean} {any any ... -> any} list -> list
   \"f\" is called for and with every list of elements that consecutively matched \"filter-f\". at least two elements at a time"
  (fold-span filter-f (l (e r) (if (< 1 (length e)) (pair (apply f e) r) (append e r))) a))

(define (map-span filter-f f a)
  "procedure:{any -> any/false} procedure:{any any ... -> any} list -> list
   apply \"f\" to each list of elements that consecutively matched \"filter-f\".
   an unpredictable number of arguments might be passed to f. with (lambda a body ...) a single list can still be accessed.
   this allows for things like (map-span string? string-append a)"
  (fold-span filter-f (l (e r) (pair (apply f e) r)) a))

(define (map-unless f stop? default . a)
  "procedure stop? list -> list/boolean:false
   {any -> any} {any -> boolean} list -> list/boolean
   map unless \"stop?\" is true for a mapping-result. return an empty list or \"default\" if \"stop?\" was true"
  (if (any null? a) (list)
    (let loop ((rest (map tail a)) (e (apply f (map first a))) (init (list)))
      (if (stop? e) default
        (if (any null? rest) (reverse (pair e init))
          (loop (map tail rest) (apply f (map first rest)) (pair e init)))))))

(define (map-with-index f . a) "procedure:{integer:index any:element ... -> any} list ... -> list"
  (let loop ((rest a) (index 0))
    (if (any null? rest) (list)
      (pair (apply f index (map first rest)) (loop (map tail rest) (+ 1 index))))))

(define (map-fold f a . init)
  "procedure list any ... -> list any ...
   procedure:{(list-element state ...) -> (list-element state ...)}
   combination of map and fold.
   call f with each list element and state values, which are set to init for the first call.
   each call to f must return a list of: the mapped result element and one
   element for each updated value of state.
   example: (map-fold (l (a index) (list (+ a index) (+ 1 index))) (list 1 2 3) 0)"
  (apply fold-multiple (l (b result-list . state) (pair (apply f b state) result-list))
    a (list) init))

(define (map-integers count f)
  "integer procedure:{integer -> any} -> list
   map over integers from 0 to count - 1"
  (let loop ((n 0)) (if (= n count) null (pair (f n) (loop (+ 1 n))))))

(define (fold-integers count init f)
  "integer any {integer any -> any} -> any
   fold over integers from 0 to count - 1"
  (let loop ((n 0) (r init)) (if (< n count) (loop (+ 1 n) (f n r)) r)))

(define (pair-fold-multiple f a . init)
  "{pair any -> any} list any ... -> any
   like fold-multiple but calling f with the pairs of list"
  (if (null? a) init (apply pair-fold-multiple f (tail a) (apply f a init))))

(define (pair-map f a)
  "procedure list -> list
   like map but not the list elements are passed to \"f\" but the pairs of the list.
   for example (1 2 3) is just another notation for the pair notation (1 . (2 . (3 . ())))
   instead of mapping (1 2 3) pair-map maps ((1 2 3) (2 3) (3))"
  (let loop ((rest a)) (if (null? rest) (list) (pair (f rest) (loop (tail rest))))))

(define (pair-reverse a)
  "pair -> pair
   reverse the order of values in a pair.
   example: (pair-reverse (pair 1 2)) -> (2 . 1)"
  (pair (tail a) (first a)))

(define (pair->list a) "pair -> list" (list (first a) (tail a)))

(define (produce f . a)
  "procedure:{any ... -> any} list ... -> list
   apply \"f\" to each ordered combination of elements from all lists, the cartesian product,
   and return the results in a list.
   for example (produce f (1 2) (4 5) (6)) is equivalent to ((f 1 4 6) (f 1 5 6) (f 2 4 6) (f 2 5 6))"
  "example of a less featureful implementation: (define (produce f a b) (map (l (a) (map (l (b) (f a b)) b)) a))\n     algorithm: append each entry of each list once to arguments and apply f.\n     loop results are nested map results except when at the last list"
  (let loop ((a a) (arguments null))
    (if (null? a) (apply f arguments)
      (let (rest (tail a))
        ((if (null? rest) map append-map) (l a (loop rest (append arguments a))) (first a))))))

(define (produce-unless f stop? default a b)
  "{any any -> any} {any -> boolean} any list list -> false/any
   produce two lists unless \"stop?\" is true for a production-result. if stop? is true, result in false"
  (append-map-unless (l (a) (map-unless (l (b) (f a b)) stop? #f b)) not default a))

(define* (replace-value a search-value replacement #:optional (equal-f equal?))
  "list any any [procedure:{any any -> boolean}] -> list"
  (replace a (l (b) (equal-f b search-value)) replacement))

(define* (replace a select? replacement) "list procedure any -> list"
  (map (l (b) (if (select? b) replacement b)) a))

(define (simplify a)
  "any/list -> list/pair/any
   list with one element -> element
   list with two non-pair elements -> pair"
  (if (list? a)
    (case (length a)
      ((1) (simplify (first a)))
      ( (2)
        (let ((first-ele (first a)) (second-ele (tail a)))
          (if (or (pair? first-ele) (pair? second-ele)) a (pair (first a) (first (tail a))))))
      (else a))
    a))

(define (simplify-list a)
  "list -> list
   examples:
     (((1 2))) -> (1 2)
     (((1 2) (3))) -> ((1 2) (3))
   removes extra nesting"
  (if (null? a) a (if (and (null? (tail a)) (list? (first a))) (simplify-list (first a)) a)))

(define (splice predicate a)
  "{list -> boolean} list -> list
   splice elements that are lists and match predicate"
  (fold-right
    (l (a result) (if (list? a) ((if (predicate a) append pair) a result) (pair a result))) (list) a))

(define (splice-last-list a)
  "list -> list
   if the last element is a list, append it to the previous elements.
   example: (splice-last-list (1 2 (3 4))) -> (1 2 3 4)"
  (match a ((e ... (? list? last-e)) (append e last-e)) (_ a)))

(define* (split-at-value a search-value #:optional inclusiveness)
  "list any [symbol:exclusive/inclusive] -> (list:left list:right)"
  (iterate-three-stop-end (l (prev e next . r) (equal? e search-value))
    (l (prev e next . r)
      (if (equal? e search-value)
        (if (equal? (q inclusive) inclusiveness) (list (reverse (pair e prev)) next)
          (list (reverse prev) (pair e next)))
        (list prev (pair e next))))
    (l (prev e next . r) r) a))

(define (split-by-pattern-take-ellipsis a rest-pattern-length)
  (if (= 0 rest-pattern-length) (list a (list))
    (let (a-len (length a))
      (if (< a-len rest-pattern-length) (list #f #f)
        (call-with-values (nullary (split-at a (- a-len rest-pattern-length))) list)))))

(define (split-by-pattern-match-ellipsis rest-pattern expr cont)
  (apply (l (match rest-expr) (cont match rest-expr rest-pattern))
    (split-by-pattern-take-ellipsis expr (length rest-pattern))))

(define (split-by-pattern-loop pattern expr prev-name prev-value r)
  "-> (matches result)
   the first pattern has already been matched and is passed with prev-name"
  (cond
    ((null? pattern) (list (reverse (pair (pair prev-name prev-value) r)) expr))
    ( (equal? (q ...) (first pattern))
      (split-by-pattern-match-ellipsis (tail pattern) expr
        (l (match rest-expr rest-pattern)
          (if match
            (if (or (null? rest-expr) (null? pattern))
              (split-by-pattern-loop rest-pattern rest-expr prev-name (pair prev-value match) r)
              (split-by-pattern-loop (tail rest-pattern) (tail rest-expr)
                (first rest-pattern) (first rest-expr)
                (pair (pair prev-name (pair prev-value match)) r)))
            (list #f #f)))))
    ( (null? expr)
      (if (and (= 2 (length pattern)) (equal? (q ...) (second pattern)))
        (split-by-pattern-loop (list) expr prev-name prev-value r) (list #f #f)))
    (else
      (split-by-pattern-loop (tail pattern) (tail expr)
        (first pattern) (first expr) (pair (pair prev-name prev-value) r)))))

(define (split-by-pattern pattern a)
  "(symbol symbol/ellipsis:... ...) list -> (list:((key . values) ...):matches list:rest)
   basic matcher that only supports matching single or repeated elements with multiple ellipses.
   creates alist elements for variables in pattern that match elements in list \"a\".
   the result is a list with two values: one for the match and one for the unmatched rest.
   if pattern did not match, then both values are false. if pattern is null, matches is null and rest is the input list.
   unlike other pattern matchers, \"pattern\" is a list and not syntax and so can be passed as a variable.
   # example
     (split-by-pattern (quote (a b ... c)) (list 1 2 3 4)) -> (((a . 1) (b 2 3) (c . 4)) ())"
  (if (null? pattern) (list (list) a)
    (if (null? a) (list #f #f)
      (split-by-pattern-loop (tail pattern) (tail a) (first pattern) (first a) (list)))))

(define (pattern-match-min-length a)
  "list -> integer
   takes a flat list with symbols and ellipses and counts the required parts of a pattern with
   symbols interpreted as matching any element and ellipses to match zero or many occurences of the previous element.
   # examples
     ((a ...)) -> 0
     ((a a ...)) -> 1
     ((a ... b ...)) -> 0
     ((a ... b ... c d)) -> 2"
  (first
    (iterate-three
      (l (p e n count)
        (list
          (if (not (or (equal? (q ...) e) (and (not (null? n)) (equal? (q ...) (first n)))))
            (+ count 1) count)))
      a 0)))

(define* (consecutive f a #:optional (c list))
  "procedure:{any -> any/boolean} list [procedure] -> (list:matches list:rest)
   splits the list into two lists, the first being a list of all beginning elements of \"a\" that consecutively matched
   \"f\", the second being the rest.
   like srfi-1 span but the result is a list and not multiple return values"
  (let loop ((a a) (result null))
    (if (and (not (null? a)) (f (first a))) (loop (tail a) (pair (first a) result))
      (c (reverse result) a))))

(define (group-split-at-matches start-group? a)
  "procedure:{any -> boolean} list -> (list ...)
   wrap consecutive elements in lists. elements for which \"start-group?\" is true become the first element of a new list.
   example
   (group-split-at-matches integer? (list \"a\" \"b\" 1 \"c\" \"d\" 2 \"e\"))
   ->
   ((\"a\" \"b\") (1 \"c\" \"d\") (2 \"e\"))"
  (let (not-start-group? (negate start-group?))
    (let loop ((rest a))
      (if (null? rest) rest
        (apply (l (matches rest-2) (pair (pair (first rest) matches) (loop rest-2)))
          (consecutive not-start-group? (tail rest)))))))

(define (insert-second a b)
  "any list -> list
   insert \"a\" as the second element into list \"b\""
  (pair (first b) (pair a (tail b))))

(define (tail-or-null a)
  "list -> list
   return the tail of list or null if there is no tail which is
   the case when list is null"
  (if (null? a) a (tail a)))

(define alist-prepend acons)

(define list->alist
  (let (proc (l (a alt prev r) (if alt (list #f #f (alist-prepend prev a r)) (list #t a r))))
    (lambda (lis)
      "-> alist
       create an association list from the given arguments,
       mapping each list element alternating to a key and value."
      (if (null? lis) lis
        (let (r (fold-multiple proc (tail lis) #t (first lis) (list)))
          (reverse!
            (if (first r) (pair (list (list-ref r 1)) (first (tail (tail r))))
              (first (tail (tail r))))))))))

(define (alist . key/value)
  "key/value ... -> alist
   create an association list from the given arguments,
   mapping each argument alternatingly to key and value.
   (alist (quote a) 1 \"b\" 2 (quote c) 3)"
  (list->alist key/value))

(define (alist-set a key value)
  "list any any -> list
   add or update an entry in an association list"
  (let loop ((rest a))
    (if (null? rest) (pair (pair key value) rest)
      (let (e (first rest))
        (if (equal? key (first e)) (pair (pair key value) (tail rest)) (pair e (loop (tail rest))))))))

(define-syntax-rules alist-ref ((a k d) ((l (r) (if r (tail r) d)) (assoc k a)))
  ((a k) (assoc-ref a k)))

(define* (group a #:optional (accessor identity))
  "list [procedure:{any -> any}] -> ((any:group-key any:group-value ...):group ...)
   groups entries by unique result values of accessor.
   by default accessor is identity and groups equal elements.
   returns an association list with one entry for each group with the value as key and related values as value"
  (let loop ((rest a) (groups (alist)))
    (if (null? rest) (map (l (a) (pair (first a) (reverse (tail a)))) groups)
      (let* ((a (first rest)) (key (accessor a)) (group (alist-ref groups key)))
        (loop (tail rest) (alist-set groups key (if group (pair a group) (list a))))))))

(define* (group-recursively a #:optional (accessor first))
  "((any ...) ...) [procedure] -> list
   group lists and the elements of groups until no further sub-groups are possible.
   the default accessor is \"first\".
   # example
       (group-recursively (list (list 1 2 3) (list 1 2 6) (list 1 4 7) (list 8 9)) first)
       -> ((1 (2 3 6) (4 7)) (8 9))
   note in the example input how the entries after 1 2 have been grouped into (2 3 6)
   # example use case
   converting a list of filesystem paths split at slashes to a nested list where prefixes are directories"
  (map
    (l (a) "(group-name element ...)"
      (let (rest (map tail (remove (compose null? tail) (tail a))))
        (if (null? rest) (first a) (pair (first a) (group-recursively rest)))))
    (group a accessor)))

(define* (list-ref-random a #:optional (random-state *random-state*))
  "list -> any
   retrieve a random element of a list"
  (list-ref a (random (length a) random-state)))

(define* (list-ref-randomize-cycle a #:optional (random-state *random-state*))
  "list -> procedure:{-> any}
   gives a procedure that when called gives the next element from a randomized version of \"a\"
   when the end of the list has been reached, the list is reset to a newly randomized version of \"a\""
  (let ((a-length (length a)) (new (randomize a random-state)) (old (list)))
    (letrec
      ( (loop
          (l ()
            (if (null? new)
              (begin (set! new (randomize old random-state)) (set! old (list)) (loop))
              (let (r (first new)) (set! new (tail new)) (set! old (pair r old)) r)))))
      loop)))

(define* (randomize a #:optional (random-state *random-state*))
  "list -> list
   return a new list with the elements of list in random order.
   algorithm: connect a random number to each element, re-sort list corresponding to the random numbers."
  (let (length-a (length a))
    (map tail
      (list-sort (l (a b) (< (first a) (first b)))
        (map (l (c) (pair (random length-a random-state) c)) a)))))
