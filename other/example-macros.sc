(sc-define-syntax (for-i index limit body ...)
  (for ((set index 0) (< index limit) (set+ index 1)) body ...))

(sc-define-syntax* (define-array name type values ...)
  "define an array with data on the stack, setting value dimensions automatically.
   example:
     (define-array a int (0 0 0 0) (0 0 0 0))
     ->
     int _t1[2][4] = { { 0, 0, 0, 0 }, { 0, 0, 0, 0 } };"
  (let
    ( (dimensions
        (let loop ((a values))
          (if (null? a) null
            (let ((b (first a)))
              (pair (length a)
                (if (and (list? b) (not (and (symbol? (first b)) (sc-syntax? (first b))))) (loop b)
                  null)))))))
    (list (q declare) name (pairs (q array) type dimensions values))))

(sc-define-syntax* (define-nested-array name type values ...)
  "define an array of arrays with data on the stack. calculates size and dimensions automatically.
   example:
     (define-nested-array a int (0 0 0 0) (0 0 0 0))
     ->
     int _t1[2][4] = { { 0, 0, 0, 0 }, { 0, 0, 0, 0 } };
     int* a[2] = { _t1[0], _t1[1] };"
  (let
    ( (id (sc-gensym))
      (nested?
        (lambda (a)
          (and (not (null? a))
            (let ((a (first a))) (and (list? a) (or (null? a) (not (sc-syntax? (first a))))))))))
    (if (nested? values)
      (list (q begin) (pairs (q define-array) id type values)
        (pairs (q define-array) name
          (symbol-append type (q *))
          (map-with-index (lambda (index value) (list (q array-get) id index)) values)))
      (pairs (q define-array) name type values))))

(sc-define-syntax* (define-array-set* name type values ...)
  "declare an array and set the values one by one. combination of declare and array-set*.
   example:
     (define-array-set* a int 3 4)
     ->
     int a[2];
     a[0] = 3;
     a[1] = 4;"
  (list (q begin) (list (q declare) name (list (q array) type (length values)))
    (pairs (q array-set*) name values)))

(sc-define-syntax* (array-get-flat a (d ...) i ...)
  "pointer access like with multidimensional array notation x[1][2][2].
   dimensions: (2 4 3), access: (1 2 2), offset: (1 * 4 * 3 + 2 * 3 + 2)"
  (qq
    (pointer-get
      (+ a
        (unquote-splicing
          (let loop ((d (tail d)) (i i))
            (if (or (null? i) (null? d)) (list (first i))
              (pair (pairs (q *) (first i) d) (loop (tail d) (tail i))))))))))
