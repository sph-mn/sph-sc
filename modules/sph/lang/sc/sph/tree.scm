; Copyright (C) 2010-2020 sph <sph@posteo.eu>
; This program is free software; you can redistribute it and/or modify it
; under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details.
; You should have received a copy of the GNU General Public License
; along with this program; if not, see <http://www.gnu.org/licenses/>.
(define-module (sph lang sc sph tree))

(export denoted-tree->prefix-tree denoted-tree->tree
  denoted-tree->tree-inner denoted-tree-adjust-depth
  denoted-tree-minimize-depth prefix-tree->denoted-tree
  prefix-tree->infix-tree prefix-tree->paths
  prefix-tree->relations prefix-tree-context-match
  prefix-tree-map prefix-tree-map-c
  prefix-tree-map-c-depth prefix-tree-map-depth
  prefix-tree-map-depth-flat prefix-tree-map-with-context
  prefix-tree-produce prefix-tree-produce-with-context
  prefix-tree-produce-with-context-mm prefix-tree-product
  prefix-tree-product-mm prefix-tree-replace-prefix
  sph-tree-description tree->denoted-tree
  tree-any tree-contains-all?
  tree-contains-some-not? tree-contains-some?
  tree-contains? tree-each
  tree-each-leaf tree-every
  tree-extract tree-filter
  tree-filter-flat tree-filter-leafs
  tree-filter-lists tree-find
  tree-finder tree-fold
  tree-fold-depth tree-fold-right
  tree-fold-right-depth tree-map
  tree-map-depth tree-map-depth-flat
  tree-map-leafs tree-map-lists
  tree-map-lists-depth tree-map-lists-self
  tree-map-self tree-map-with-state
  tree-mapper tree-mapper-produce
  tree-mapper-top tree-produce
  tree-produce-lists tree-produce-lists-depth
  tree-replace-at-once tree-replace-by-list
  tree-splice tree-transform tree-transform* tree-transform-ascend tree-transform-descend-identity)

(use-modules (sph lang sc sph) ((sph lang sc sph alist) #:select (alist-ref))
  ((sph lang sc sph list) #:select (contains? fold-integers flatten fold-multiple fold-segments))
  ((srfi srfi-1) #:select (append-map find any fold every fold-right last)))

(define sph-tree-description
  "process tree-like list structures.
   # highlights
   prefix-tree->paths
   # other
   denoted
     a representation using pairs where the first element denotes the nesting level and the second element is content
     (a b (c (d e)) f)-> ((0 . a) (1 . b) (2 . c) (3 . d) (3 . e) (0 . 4))
   prefix-tree
     first elements of lists interpreted as parent of tail elements. similar to scheme code for procedure application
     (a b (c (d e)) f)
     example with nesting marked by indent
       a
         b
         c
           d
             e
         f")

(define (tree-mapper map f a)
  "procedure:{procedure list ... -> list} procedure:{element -> new-element} list -> list
   map over lists and sub-lists using the given map procedure, which needs to have the same
   signature as \"map\".
   maps from bottom to top"
  (map (l (a) (if (list? a) (f (tree-mapper map f a)) (f a))) a))

(define (tree-mapper-top map f a)
  "procedure:{procedure list ... -> list} procedure:{element -> new-element} list -> list
   maps from top to bottom.
   list results of f will be entered for further mapping.
   this procedure is used to map sub-lists before eventually
   mapping their elements if the map result is a list"
  (map (l (a) (let (b (f a)) (if (list? b) (tree-mapper-top map f b) b))) a))

(define (tree-finder find f a)
  "procedure:{predicate list ... -> any} procedure:{element -> any/boolean} list -> any
   call f with each tree list and leaf from top to bottom and return true results of f"
  (find (l (a) (or (f a) (and (list? a) (tree-finder find f a)))) a))

(define (tree-find predicate a)
  "procedure:{element -> any/boolean} list -> any
   find a sub-tree that contains an element that matches predicate"
  (tree-finder find predicate a))

(define (tree-any f a)
  "procedure list -> false/any
   call f with each tree element list or leaf from top to bottom and return the first true result of f.
   can be used to extract single elements from tree. aliased as tree-any and tree-extract"
  (tree-finder any f a))

(define (tree-every f a) (not (tree-any (negate f) a)))
(define tree-extract tree-any)

(define (tree-each f a)
  "procedure:{any ->} list ->
   call f for every element in tree, lists and non-lists.
   list elements left to right, nested lists bottom to top.
   tree-for-each"
  (each (l (e) (if (list? e) (begin (tree-each f e) (f e)) (f e))) a))

(define (tree-each-leaf f a)
  "procedure:{any ->} list ->
   call f for each non-list element in tree"
  (each (l (b) (if (list? b) (tree-each-leaf f b) (f b))) a))

(define (tree-fold p r t)
  "procedure:{any:element any:result -> any:result} any:result list:tree -> any
   fold over all lists and non-list elements in tree
   elements from left to right, nested lists bottom to top.
   lists passed to f will be fold results from list elements that have already been processed.
   see tree-fold-right for examples"
  (fold (l (a r) (if (list? a) (p (tree-fold p (list) a) r) (p a r))) r t))

(define (tree-fold-right p r t)
  "procedure:{any:element any:result -> any:result} any:result list:tree -> any:result
   fold over all list and non-list elements in tree.
   elements from left to right, nested lists bottom to top
   example 1
     (tree-fold-right
       (l (a r) (pair a r))
       (list)
       (q (1 2 3 (4 5 (6 7) (8 9 10)))))
     ->
     (q (1 2 3 (4 5 (6 7) (8 9 10))))
   example 2
     (tree-fold-right
       (l (a r) (if (list? a) a (if (even? a) (pair a r) r)))
       (list) (q (1 2 3 (4 5 (6 7) (8 9 10)))))
     ->
     (q (1 2 3 (4 5 (6 7) (8 9 10))))"
  (fold-right (l (a r) (if (list? a) (p (tree-fold-right p (list) a) r) (p a r))) r t))

(define* (tree-fold-depth p r t #:optional (n 1) (inc 1+))
  "procedure:{element result integer:depth} any tree [integer:depth procedure:{integer -> integer}] -> any
   like tree-fold but keeps track of the current nesting depth"
  (fold (l (a r) (if (list? a) (p (tree-fold-depth p (list) a (inc n) inc) r n) (p a r n))) r t))

(define* (tree-fold-right-depth f r t #:optional (depth 1) (inc 1+))
  "procedure:{any:element any:result -> any:result} any:result list:tree -> any
   fold over all list and non-list elements in tree.
   elements from left to right, nested lists bottom to top"
  (fold-right
    (l (b r)
      (if (list? b) (f (tree-fold-right-depth f (list) b (inc depth) inc) r depth) (f b r depth)))
    r t))

(define (tree-filter predicate a)
  "procedure:{element -> any/boolean} list -> list
   call predicate for each non-list and list element in tree and
   only keep the elements for which predicate returned true.
   elements left to right, nested lists bottom to top.
   example
     (tree-filter (l (a) (or (list? a) (even? a))) (q (1 2 3 (4 5 (6 7) (8 9 10)))))
     ->
     (2 (4 (6) (8 10)))"
  (tree-fold-right (l (a r) (if (predicate a) (pair a r) r)) null a))

(define (tree-filter-leafs predicate a)
  "like tree-filter but calls predicate only for non-list elements"
  (tree-filter (l (a) (or (list? a) (predicate a))) a))

(define (tree-filter-lists predicate a)
  "like tree-filter but calls predicate only for list elements.
   example - keep only lists with more than 3 elements
     (tree-filter-lists (l (a) (< 3 (length a))) (q (1 2 3 (4 5 (6 7) (8 9 10)))))
     ->
     (1 2 3)"
  (tree-filter (l (a) (if (list? a) (predicate a) #t)) a))

(define (tree-map f a)
  "procedure:{any -> any} list -> list
   maps elements left to right and nested lists bottom to top.
   not map the topmost tree structure itself."
  (tree-mapper map f a))

(define (tree-map-lists f a)
  "{list -> any} list -> list
   like tree-map but pass only the lists in tree to f, skipping and keeping non-list elements. bottom-to-top"
  (tree-mapper map (l (a) (if (list? a) (f a) a)) a))

(define (tree-map-leafs f a)
  "procedure:{any -> any} list -> list
   call f only with non-list elements, all other elements are kept"
  (tree-mapper map (l (a) (if (list? a) a (f a))) a))

(define (tree-map-lists-self f a)
  "{list -> any} list -> list
   like tree-map-and-self but calls f only for list elements"
  (f (tree-map-lists f a)))

(define (tree-map-self f a)
  "{any -> any} list -> list
   like tree-map-lists but also map the given list itself"
  (f (tree-map f a)))

(define* (tree-map-depth f a #:optional (initial-depth 0))
  "procedure:{any:element integer:depth} list [integer] -> list
   like tree-map but also passes a number for the current nesting depth to f"
  (let loop ((rest a) (depth initial-depth) (r (list)))
    (if (null? rest) (f (reverse r) depth)
      (loop (tail rest) depth
        (pair
          (if (list? (first rest)) (loop (first rest) (+ 1 depth) (list)) (f (first rest) depth)) r)))))

(define* (tree-map-lists-depth f a #:optional (depth-init 1) (map-depth 1+))
  "{list integer:depth -> any} list [integer {integer -> integer:next-depth}] -> list
   like tree-map-lists with additional arguments for the current nesting depth"
  (let loop ((e a) (depth depth-init))
    (map (l (e) (if (list? e) (f (loop e (map-depth depth)) depth) e)) e)))

(define (tree-map-with-state f a . init)
  "{any any:custom-state-value ... -> list:custom-state-values} -> (list:mapped-elements any:custom-state-value ...)
   like tree-map but can carry and update a number of custom values per call, similar to fold-multiple"
  (apply (l (r . state) (apply list (reverse r) state))
    (apply fold-multiple
      (l (e r . state)
        (apply (l (map-r . state) (apply list (pair map-r r) state))
          (if (list? e) (apply tree-map-with-state f e state) (apply f e state))))
      a (list) init)))

(define (tree-map-depth-flat f a . initial-depth)
  "procedure:{integer:depth any:element -> any:result-element} list [integer] -> (any ...)
   map elements of tree to a flat list. apply f with a number
   stating how deeply the element is nested in other lists and the current element"
  (reverse
    (let loop ((rest a) (depth (if (null? initial-depth) 0 (first initial-depth))) (r (list)))
      (if (null? rest) r
        (loop (tail rest) depth
          (if (list? (first rest)) (loop (first rest) (+ 1 depth) r)
            (pair (f depth (first rest)) r)))))))

(define (tree-produce f a b)
  "procedure:{any any -> any} list list -> any
   apply f with every possible ordered combination of list and non-list elements of trees.
   traverses like tree-map"
  (tree-map (l (a) (tree-map (l (b) (f a b)) b)) a))

(define (tree-produce-lists f a b) "like tree-produce only for list elements"
  (tree-map-lists (l (a) (tree-map-lists (l (b) (f a b)) b)) a))

(define (tree-mapper-produce map f a b)
  "procedure:{f list:elements -> any} procedure:{any:element-a any:element-b -> any}:f list list -> any
   call f with each ordered combination between elements of two lists with a
   map procedure that is called nested like
   (each (lambda (a) (each (lambda (b) (f a b)) b)) a)"
  (map
    (l (a)
      (if (list? a) (tree-mapper-produce map f a b)
        (map (l (b) (if (list? b) (tree-mapper-produce map f a b) (f a b))) b)))
    a))

(define* (tree-produce-lists-depth f a b #:optional (inc 1+) (depth-init 1))
  "{any:element-a any:element-b integer:depth-a integer:depth-b} list:list-a list:list-b [{integer -> integer} integer] -> list"
  (tree-map-lists-depth
    (l (e-1 depth-1)
      (tree-map-lists-depth (l (e-2 depth-2) (f e-1 e-2 depth-1 depth-2)) b inc depth-init))
    a inc depth-init))

(define (tree-replace-by-list a replace? replacements)
  "list {any -> boolean} (any ...) -> list
   replace each non-list element in tree that matches replace? with the next element from replacements.
   it is an error if there are more replacements than matches"
  (if (null? a) a
    (if (null? replacements) replacements
      (first
        (let loop ((rest a) (r (list)) (replacements replacements))
          (if (null? rest) (list (reverse r) replacements)
            (let (a (first rest))
              (if (list? a)
                (apply (l (a replacements) (loop (tail rest) (pair a r) replacements))
                  (loop a (list) replacements))
                (if (replace? a)
                  (loop (tail rest) (pair (first replacements) r) (tail replacements))
                  (loop (tail rest) (pair a r) replacements))))))))))

(define (tree-replace-at-once predicate f a)
  "procedure:{element -> boolean} procedure:{list:matched-elements -> list} list -> list
   searches through tree recursively, collecting all elements (including lists) that match collect-proc, then calls
   f with a list of matched elements. the result of f must of length zero (no replacement) or matched-element-count (replaces all matches).
   results in the tree with the matched elements are replaced in order by the result elements from calling f"
  (tree-replace-by-list a predicate (f (tree-filter-flat predicate a))))

(define (tree-filter-flat predicate a)
  "procedure:{any -> boolean/any} list -> list
   results in a flat list of all list and non-lists elements of tree
   for which predicate returned true"
  (let loop ((rest a) (r (list)))
    (if (null? rest) r
      (loop (tail rest)
        (let (b (first rest)) (if (predicate b) (pair b r) (if (list? b) (loop b r) r)))))))

(define (tree-splice predicate a)
  "prodecure:{element -> boolean/any} list -> list
   merge nested lists that match predicate with their parent list.
   example
     (tree-splice (l (a) (= 3 (length a))) (q (1 2 3 (4 5 (6 7) (8 9 10)))))
     ->
     (1 2 3 (4 5 (6 7) 8 9 10))"
  (tree-fold-right
    (l (a result) (if (list? a) ((if (predicate a) append pair) a result) (pair a result))) null a))

; -- tree-contains

(define* (tree-contains? a search-value #:optional (equal? equal?))
  "list any [procedure:{any any -> boolean}] -> boolean
   compares all list and non-list elements with search-value and returns true on a match"
  (any (l (a) (or (equal? a search-value) (and (list? a) (tree-contains? a search-value)))) a))

(define (tree-contains-some? a equal? . search-values)
  "list {any any -> boolean} any ... -> boolean
   like tree-contains? and true if any of the search-values is found"
  (any (l (search-value) (tree-contains? a search-value equal?)) search-values))

(define (tree-contains-all? a equal? . search-values)
  "list {any any -> boolean} any ... -> boolean
   like tree-contains? but true only if every of the search-values has been found"
  (every (l (search-value) (tree-contains? a search-value equal?)) search-values))

(define* (tree-contains-some-not? a equal? . search-values)
  "list {any any -> boolean} any ... -> boolean
   like tree-contains? but true only if any of the search-values is not contained"
  (any
    (l (e)
      (if (list? e) (apply tree-contains-some-not? e search-values)
        (not (contains? search-values e equal?))))
    a))

; -- tree-transform

(define (tree-transform-ascend rest leaf-list recurse-descend ascend terminal states)
  (if (null? rest) (apply ascend (reverse leaf-list) states)
    (let (a (first rest))
      (apply
        (l (a . states)
          (tree-transform-ascend (tail rest) (pair a leaf-list)
            recurse-descend ascend terminal states))
        (apply (if (list? a) recurse-descend terminal) a states)))))

(define (tree-transform* a descend ascend terminal . states)
  "list procedure procedure procedure any ... -> any
   descend :: any:element procedure:recurse-descend any:state ... -> (any:result-element boolean:continue? any:state ...)
   ascend :: any:element any:state ... -> (any:result-element any:state ...)
   terminal :: any:element any:state ... -> (any:result-element any:state ...)
   like tree-transform but also takes, passes and updates caller specified values"
  (letrec
    ( (recurse-descend
        (lambda (a . states)
          (if (and (list? a) (not (null? a)))
            (apply
              (l (r continue? . states)
                (if continue?
                  (if r (apply recurse-descend r states)
                    (tree-transform-ascend a (list) recurse-descend ascend terminal states))
                  (pair r states)))
              (apply descend a recurse-descend states))
            (apply terminal a states)))))
    (apply recurse-descend a states)))

(define (tree-transform a descend ascend terminal)
  "list procedure procedure procedure -> any
   descend :: any:element procedure:recurse-descend -> (any:result-element boolean:continue?)
   ascend :: any:element -> any:result-element
   terminal :: any:element -> any:result-element
   map/transform list and sub-lists first top to bottom, calling \"descend\" for each sub-list,
   then bottom to top, calling \"ascend\" for lists and \"terminal\" for non-lists/leafs.
   descend should return a list of two values: the first for the result and the second a boolean indicating if the result
   should be passed to ascend and terminal (true) or if that should be skipped (false).
   example use cases:
   * compile s-expression list trees into string output languages by mapping all expressions and sub-expressions to strings
   * replace some lists in a tree"
  (first
    (tree-transform* a (l (a re-descend) (descend a (compose first re-descend)))
      (compose list ascend) (compose list terminal))))

(define (tree-transform-descend-identity . a)
  "any ... -> (#f #t)
   a tree-transform descend procedure that does not do anything"
  (list #f #t))

; -- prefix trees

(define (prefix-tree-context-match a pattern)
  "list list -> boolean
   true if pattern exists in prefix-tree with the same tree interpretation as prefix-tree-context-produce.
   example: (a b (d c)) contains patterns (a d c) and (a b) and (a d)"
  (if (null? a) a
    (null?
      (let loop ((rest (tail a)) (prefix (first a)) (context (list)) (pattern pattern))
        (if (or (null? pattern) (null? rest)) pattern
          (let*
            ( (e (first rest))
              (pattern
                (if (and (list? e) (not (null? e)))
                  (let (prefix-next (first e))
                    (loop (tail e) prefix-next
                      (pair prefix context)
                      (if (equal? prefix (first pattern)) (tail pattern) pattern)))
                  (if (equal? prefix (first pattern))
                    (let (pattern (tail pattern))
                      (if (null? pattern) pattern
                        (if (equal? e (first pattern)) (tail pattern) pattern)))
                    pattern))))
            (loop (tail rest) prefix context pattern)))))))

(define (prefix-tree-replace-prefix a replacements)
  "list ((to-replace . replacement) ...) -> list
   replace all list prefixes, the first element of a list, in tree based on the given replacements structure"
  (tree-map
    (l (a depth)
      (if (list? a)
        (let ((replacement (alist-ref replacements (first a))))
          (if replacement (if (procedure? replacement) (replacement a) replacement) a))
        a))
    a))

(define (prefix-tree-produce f a)
  "{any ... -> any} list:prefix-tree -> list
   calls f for each combination of prefix and tail"
  (prefix-tree-produce-with-context (l (context a) (apply f (reverse (pair context a)))) a))

(define (prefix-tree->paths a)
  "list -> (string ...)
   regard tree as a nested list representation of a filesystem file and directory structure
   and return a flat list of filesystem path strings.
   example:
   (prefix-tree->paths (list \"/usr\" (list \"bin\" (list \"share\" \"guile\") \"include\") \"/var\"))
   creates
   (list
     \"/usr/bin\"
     \"/usr/share/guile\"
     \"/usr/include\"
     \"/var\")"
  (prefix-tree-produce (l a (string-join a "/")) a))

(define* (prefix-tree-product a #:optional ignore-prefixes?) "list -> list"
  "combines prefixex and tails as a one-to-many (prefix to tail elements) relation.\n    example (a (d e f) (g h i) j) -> ((a d e) (a d f) (a g h) (a g i) (a j))"
  (prefix-tree-produce-with-context (l (context a) (reverse (pair context a))) a ignore-prefixes?))

(define (prefix-tree-product-mm a) "list -> list"
  "like prefix-tree-product, but also interprets lists as prefixes as many-to-many relations\n    (many prefixes to tail elements).\n    example ((a b) c) -> ((a c) (b c))"
  (prefix-tree-produce-with-context-mm (l (context a) (reverse (pair context a))) a))

(define (prefix-tree->relations a)
  "list -> (pair ...)
   create a list of all individual relations between prefix and tail elements or tail element prefixes.
   example:
   ((a (b c (d e)))) -> ((a . b) (b . c) (b . d) (d . e))"
  (append-map
    (l (a)
      (if (list? a)
        (let ((prefix (first a)) (suffix (tail a)))
          (append-map
            (l (a)
              (pair (pair prefix (if (list? a) (first a) a))
                (if (list? a) (prefix-tree->relations (list a)) (list))))
            suffix))
        (list a)))
    a))

(define (prefix-tree-map f a)
  "{any:prefix list:tail} list -> list
   map only lists, split into prefix and tail"
  (if (null? a) a
    (let (prefix (first a))
      (f (if (list? prefix) (prefix-tree-map f prefix) prefix)
        (map (l (b) (if (list? b) (prefix-tree-map f b) b)) (tail a))))))

(define* (prefix-tree-map-depth f a #:optional (initial-depth 0))
  "{any:prefix list:tail integer:depth -> any} list [integer] -> list
   like prefix-tree-map but with an additional argument for the current nesting-depth"
  (let loop ((rest a) (depth initial-depth))
    (if (null? rest) rest
      (let (prefix (first rest))
        (f (if (list? prefix) (loop prefix (+ depth 1)) prefix)
          (map (l (e) (if (list? e) (loop e (+ depth 1)) e)) (tail rest)) depth)))))

(define* (prefix-tree->infix-tree a #:optional (prefix->infix (l (p) p)))
  "list [procedure:{any -> any}] -> list
   converts list structures like (or a b (and c d)) to (a or b or (c and d))
   the optional procedure translates prefixes"
  (prefix-tree-map
    (l (prefix values)
      ( (l (prefix) (reverse (tail (fold (l (e prev) (pair prefix (pair e prev))) (list) values))))
        (prefix->infix prefix)))
    a))

(define (prefix-tree-map-c f continue& a)
  "procedure:{prefix tail} procedure:{list procedure:f procedure:continue:{list ->}} list -> list
   maps over only the lists, split into prefix and tail.
   the procedure continue& gets a procedure argument that when called continues the iteration. if it is not called,
   the iteration stops and the result of continue& is the main result"
  (let loop ((rest a))
    (if (null? rest) rest
      (f (first rest) (map (l (e) (if (list? e) (continue& e f loop) e)) (tail rest))))))

(define* (prefix-tree-map-c-depth f continue a #:optional (inc 1+) (depth-init 1))
  "{any:prefix list:tail any} {list procedure:f {list ->} any}:continue list [{integer -> integer}] -> list
   like prefix-tree-map-c but with additional arguments for the current nesting-depth"
  (let loop ((rest a) (depth depth-init))
    (if (null? rest) rest
      (f (first rest) (map (l (a) (if (list? a) (continue a f loop (inc depth)) a)) (tail rest))
        depth))))

(define prefix-tree-map-with-context
  (letrec
    ( (loop
        (l (f a context) "{any list:parent-prefixes -> any} list list:initial-context -> list"
          (if (null? a) a
            (f (first a)
              (map (l (b) (if (list? b) (loop f b (pair (first a) context)) b)) (tail a)) context)))))
    (l (f a)
      "{any list:upper-prefixes} list -> list
       like (prefix-tree-map) but with an additional argument for parent prefixes"
      (loop f a (list)))))

(define* (prefix-tree-produce-with-context f a #:optional ignore-prefixes)
  "{element context -> any} list -> list
   context is a list containing nested-list prefixes in bottom-to-top order.
   for example (a (d e f) k (g h i) j) leads to f called with each of the following arguments:
   (d (a)), (e (d a)), (f (d a)),(k (a)), (g (a)), (h (g a)), (i (g a)), (j (a))
   ignore-prefixes ignores lists that are themselves prefixes"
  (if (null? a) a
    (let*
      ( (map-to-result (l (b prefix context r) (pair (f b (pair prefix context)) r)))
        (map-to-result-list (if ignore-prefixes (l (b prefix context r) r) map-to-result)))
      (reverse
        (let loop ((rest (tail a)) (prefix (first a)) (context (list)) (r (list)))
          (if (null? rest) r
            (loop (tail rest) prefix
              context
              (let (b (first rest))
                (if (and (list? b) (not (null? b)))
                  (let (prefix-next (first b))
                    (loop (tail b) prefix-next
                      (pair prefix context) (map-to-result-list prefix-next prefix context r)))
                  (map-to-result b prefix context r))))))))))

(define (prefix-tree-produce-with-context-mm f a)
  "procedure:{any list -> any} list -> list
   like prefix-tree-produce-with-context but lists can be used as list prefixes for many-to-many relations"
  (if (null? a) a
    (reverse
      (let loop ((rest (tail a)) (prefix (first a)) (context (list)) (result (list)))
        (if (null? rest) result
          (loop (tail rest) prefix
            context
            (let (b (first rest))
              (if (list? prefix)
                (fold
                  (if (and (list? b) (not (null? b)))
                    (let ((b-first (first b)) (b-tail (tail b)))
                      (l (prefix result)
                        (append (loop b-tail b-first (pair prefix context) (list)) result)))
                    (l (prefix result) (pair (f b (pair prefix context)) result)))
                  result prefix)
                (if (and (list? b) (not (null? b)))
                  (loop (tail b) (first b) (pair prefix context) result)
                  (pair (f b (pair prefix context)) result))))))))))

(define (prefix-tree-map-depth-flat proc a . initial-depth)
  "{integer any -> any} list [integer] -> (any ...)
   like tree->denoted-tree-->flat but the nesting-depth number corresponds to prefix-tree interpretation"
  (reverse
    (let loop
      ( (rest a) (depth (if (null? initial-depth) 0 (first initial-depth))) (is-prefix #t)
        (r (list)))
      (if (null? rest) r
        (loop (tail rest) depth
          #f
          (if (list? (first rest)) (loop (first rest) (+ 1 depth) #t r)
            (pair
              (proc (if (and is-prefix (> depth 0) (not (null? (tail rest)))) (- depth 1) depth)
                (first rest))
              r)))))))

(define (prefix-tree->denoted-tree a . initial-depth)
  "list [integer] -> list
   like tree->denoted-tree but the nesting-depth number corresponds to prefix-tree interpretation. example
   (a b (c (d e)) f) -> ((0 . a) (1 . b) (2 . c) (3 . d) (3 . e) (0 . 4))"
  (apply prefix-tree-map-depth-flat pair a initial-depth))

; -- "denoted" trees

(define (denoted-tree-minimize-depth a)
  "procedure list integer -> list
   decrease nesting-depth for all element until at least one element has a nesting-depth of zero"
  (if (null? a) a
    (let (min-depth (apply min (map first a)))
      (if (zero? min-depth) a (denoted-tree-adjust-depth a - min-depth)))))

(define (tree->denoted-tree a . initial-depth)
  "list [integer] -> list
   convert a tree to an association list where each element is a list having a nesting-depth number
   as the first element. similar to this is the usage of indentation for nesting depth.
   (a b (c (d e)) f) -> ((0 . a) (0 . b) (1 . c) (2 . d) (2 . e) (0 . f))"
  (apply tree-map-depth-flat pair a initial-depth))

(define (denoted-tree-adjust-depth a operator . arguments)
  "procedure list integer -> list
   map the nesting-depth of each element with operator and arguments.
   the lowest possible depth is bounded to zero.
   example:
   (denoted-tree-adjust-depth a + 1)"
  (map (l (a) (pair (max 0 (apply operator (first a) arguments)) (tail a))) a))

(define-syntax-rule (denoted-tree->tree-inner a depth-start r-2 r update-r ...)
  ; todo: dispense of this syntax
  (first
    (let loop ((rest a) (depth depth-start) (r (list)))
      (if (null? rest) (list (reverse r))
        (let* ((b (first rest)) (depth-2 (first b)))
          (if (< depth depth-2)
            (apply (l (r-2 . rest) (loop rest depth update-r ...)) (loop rest (+ 1 depth) (list)))
            (if (> depth depth-2) (pair (reverse r) rest)
              (loop (tail rest) depth (pair (tail b) r)))))))))

(define* (denoted-tree->prefix-tree a #:optional (depth-start 0))
  "list [integer] -> list
   convert a tree representation like this ((0 a) (1 b) (2 c) (1 d)) to this (a (b c) d).
   cases like ((0 a) (3 b) (0 c)) are converted to (a ((b)) c)"
  (denoted-tree->tree-inner a depth-start
    r-2 r (if (null? r) (pair r-2 r) (pair (pair (first r) r-2) (tail r)))))

(define* (denoted-tree->tree a #:optional (depth-start 0))
  "list:((integer any ...) ...) [integer] -> list
   convert a tree representation like this ((0 a) (1 b) (2 c) (1 d)) to this (a (b (c) d))"
  (denoted-tree->tree-inner a depth-start r-2 r (pair r-2 r)))
