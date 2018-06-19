
; load all expressions from sc tests as if they were defined in an sc file,
; then extract documentation from this

(tree-extract
    (l (a)
      (match a
        ( ( (quote test-execute-procedures-lambda) tests ...)
          (display-line
            (string-join
              (apply append
                (filter-map
                  (l (test)
                    (and (list? test) (map-slice 2 (l (a b) (format-test a b)) (tail test))))
                  tests))
              "\n\n")))
        (_ #f)))
    (file->datums path))