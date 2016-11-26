(library (sph lang c expressions)
  (export
    c-address-of
    c-apply
    c-compound
    c-convert-type
    c-define
    c-define-array
    c-function
    c-function-pointer
    c-identifier
    c-if
    c-if-statement
    c-line
    c-parameter
    c-pointer
    c-pointer-deref
    c-set
    c-statement
    c-string
    c-stringify
    c-struct-get
    c-typedef
    c-typedef-function
    c-value
    c-vector
    cp-concat
    cp-if
    cp-include
    cp-include-path
    cp-pre-define
    cp-stringify
    cp-undef)
  (import
    (guile)
    (rnrs base)
    (sph)
    (sph alist)
    (sph hashtable)
    (sph list)
    (sph string)
    (only (rnrs hashtables) hashtable?))

  ;generating c expressions as strings.
  (define (c-stringify a) (string-append "#" a))
  (define (cp-undef a) (string-append "#undef " a))
  (define (cp-include-path path) (string-append "#include " (c-string path)))
  (define (cp-include path) (string-append "#include <" path ">"))
  (define (cp-concat a) "(string ...) -> string" (string-join a "##"))
  (define (cp-stringify a) "string -> string" (string-append "#" a))

  (define (cp-pre-define name body parameters)
    (string-append "#define " name (if parameters parameters "") " " body))

  (define* (cp-if type test consequent #:optional alternate)
    (string-append "#"
      (if (equal? (q if) type) "if"
        (if (equal? (q ifdef) type) "ifdef"
          (if (equal? (q ifndef) type) "ifndef"
            (throw (q cannot-convert-to-c) (list (q if) type test consequent alternate)))))
      " " test
      "\n" consequent "\n" (if alternate (string-append "#else\n" alternate "\n") "") "#endif"))

  (define (c-compound a)
    "string/((string string) ...) -> string
    a: {a}
    (a): {string}
    (a b): {a,b}
    ((a b) (c d)): {.a=b,.c=d}
    ((a b) c): {.a=b,c}
    also creates compound literals"
    (string-append "{"
      (if (list? a)
        (string-join (map (l (a) (if (list? a) (string-append "." (first a) "=" (second a)) a)) a)
          ",")
        a)
      "}"))

  (define (c-typedef name a) (c-line "typedef" a name))

  (define (c-typedef-function name return-type . types)
    (string-append "typedef " (apply c-function-pointer name return-type types)))

  (define (c-convert-type a type)
    "extra round brackets ensure nestability in function pointer cases like this: (dg_pair_reader_t)((*state).reader)(state,count,result)"
    (string-append "((" type ")(" a "))"))

  (define (c-line . a) (string-join a " "))

  (define* (c-statement keyword body #:optional prefix-a suffix-a)
    "\"keyword (prefix-a) { body } (suffix-a)\""
    (string-append keyword (if prefix-a (parenthesise prefix-a) "")
      (c-compound body) (if suffix-a (parenthesise suffix-a) "")))

  (define (c-enum name enum-list)
    (string-append "enum " name
      (c-compound
        (string-join
          (map (l (e) (if (list? e) (string-append (first e) "=" (first (tail e))) e)) enum-list) ","))))

  (define (c-define-array name type sizes values) "string string (string ...) string ... -> string"
    (string-append type " "
      name (apply string-append (map (l (a) (string-append "[" a "]")) sizes))
      (if values (string-append "={" (string-join values ",") "}") "")))

  (define (c-define name type value) "string string -> string"
    (string-append type " " name (if value (string-append "=" value) "")))

  (define (c-identifier a)
    (string-append
      (cond ((symbol? a) (symbol->string a)) ((string? a) a)
        (else (throw (q cannot-convert-to-c-identifier))))))

  (define (c-identifier-list a)
    (parenthesise
      (if (list? a) (string-join (map c-identifier a) ",")
        (if (or (symbol? a) (string? a)) (c-identifier a)
          (throw (q cannot-convert-to-c-identifier))))))

  (define (c-function-parameter name type) (string-append type " " name))

  (define (c-function-parameters names types)
    (parenthesise
      (if (list? names)
        (if (equal? (length names) (length types))
          (string-join (map c-function-parameter names types) ",")
          (throw (q type-and-parameter-list-length-mismatch) names))
        (if (or (symbol? names) (string? names)) (c-function-parameter names types)
          (throw (q cannot-convert-to-c-parameter))))))

  (define* (c-function name type-output body #:optional (names (list)) (type-input (list)))
    (string-append (if type-output (string-append type-output " ") "") name
      (catch (q type-and-parameter-list-length-mismatch)
        (thunk (c-function-parameters names type-input)) (l (key . data) (apply throw key name data)))
      (if body (string-append "{" body "}") "")))

  (define* (c-apply proc-name #:optional (args "")) (string-append proc-name "(" args ")"))

  (define* (c-if test consequent #:optional alternate)
    "string string [string] -> string
    create an if expression"
    (string-append "(" test "?" consequent ":" (if alternate alternate "0") ")"))

  (define* (c-if-statement test consequent #:optional alternate)
    "string string [string] -> string
    create an if statement"
    (string-append "if(" test
      "){" consequent "}" (if alternate (string-append "else{" alternate "}") "")))

  (define (c-pointer-deref a index)
    (if index (string-append "(*(" a "+" index "))") (string-append "(*" a ")")))

  (define (c-set name value) (string-append name "=" value))
  (define (c-pointer type) (string-append type " * "))
  (define (c-address-of a) (string-append "&" a))

  (define (c-function-pointer inner type-output . type-input)
    (string-append type-output "(*" inner ")(" (string-join type-input ",") ")"))

  (define (c-struct-get a key) (string-append a "." key))

  (define c-escape-single-char
    (alist "\"" "\\\\" "\a" "\\a" "\n" "\\n" "\b" "\\b" "\f" "\\f" "\r" "\\r" "\t" "\\t" "\v" "\\v"))

  (define (c-string str)
    (string-enclose
      (fold (l (e r) (string-replace-string r (first e) (tail e))) str c-escape-single-char) "\""))

  (define (c-value a) "handles the default conversions between scheme and c types"
    (cond ((symbol? a) (symbol->string a)) ((string? a) (c-string a))
      ((number? a) (number->string a)) ((boolean? a) (if a "1" "0"))
      ((char? a) (string-enclose (any->string a) "'")) (else (throw (q cannot-convert-to-c-value) a)))))
