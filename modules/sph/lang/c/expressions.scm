(library (sph lang c expressions)
  (export
    c-address-of
    c-apply
    c-array-get
    c-bit-not
    c-comma-join
    c-compound
    c-convert-type
    c-curly-brackets
    c-define
    c-define-array
    c-for
    c-function
    c-function-pointer
    c-identifier
    c-if
    c-if-statement
    c-line
    c-not
    c-parameter
    c-pointer
    c-pointer-get
    c-set
    c-statement
    c-string
    c-stringify
    c-struct-get
    c-struct-pointer-get
    c-typedef
    c-typedef-function
    c-value
    c-variable
    c-vector
    cp-concat
    cp-define
    cp-if
    cp-include
    cp-include-path
    cp-stringify
    cp-undef
    parenthesise-ambiguous)
  (import
    (guile)
    (ice-9 regex)
    (sph)
    (sph alist)
    (sph list)
    (sph string))

  (define sph-lang-c-expressions-description "generating c expressions as strings")
  (define ambiguous-regexp (make-regexp "^(\\*|&)+|\\.|->|\\[|\\("))

  (define (parenthesise-ambiguous a)
    (if (or (parenthesised? a) (not (regexp-exec ambiguous-regexp a))) a (parenthesise a)))

  (define (parenthesise-ensure a) (if (parenthesised? a) a (parenthesise a)))
  (define (c-comma-join a) (string-join a ","))
  (define (c-curly-brackets a) (string-append "{" a "}"))
  (define (cp-undef a) (string-append "#undef " a))
  (define (cp-include-path path) (string-append "#include " (c-string path)))
  (define (cp-include path) (string-append "#include <" path ">"))
  (define (cp-concat a) "(string ...) -> string" (string-join a "##"))
  (define (cp-stringify a) "string -> string" (string-append "#" a))

  (define* (cp-define name #:optional body parameters) "string false/string false/string -> string"
    (string-append "#define " name
      (if parameters parameters "") (if body (string-append " " body) "")))

  (define* (cp-if type test consequent #:optional alternate)
    (string-replace-string
      (string-append "#"
        (case type
          ((if) "if")
          ((ifdef) "ifdef")
          ((ifndef) "ifndef")
          (else (raise (list (q sc-syntax-error) (q pre-if) type test consequent alternate))))
        " " test
        "\n" consequent "\n" (if alternate (string-append "#else\n" alternate "\n") "") "#endif")
      "\n\n" "\n"))

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

  (define (c-for init test update body)
    (string-append "for(" init ";" test ";" update "){" body "}"))

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

  (define (c-variable name type) "string string -> string" (string-append type " " name))

  (define (c-define name type value) "string string [string] -> string"
    (string-append (c-variable name type) (if value (string-append "=" value) "")))

  (define (c-identifier a)
    (string-append
      (cond
        ((symbol? a) (symbol->string a))
        ((string? a) a)
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
        (nullary (c-function-parameters names type-input))
        (l (key . data) (apply throw key name data)))
      (if body (string-append "{" body "}") "")))

  (define* (c-apply proc-name #:optional (args ""))
    (string-append (parenthesise-ambiguous proc-name) (parenthesise args)))

  (define* (c-if test consequent #:optional alternate)
    "string string [string] -> string
     create an if expression"
    (string-append "(" test "?" consequent ":" (if alternate alternate "0") ")"))

  (define* (c-if-statement test consequent #:optional alternate)
    "string string [string] -> string
     create an if statement"
    (string-append "if" (parenthesise-ensure test)
      "{" consequent "}" (if alternate (string-append "else{" alternate "}") "")))

  (define (c-array-get a . indices)
    (apply string-append (parenthesise-ambiguous a) (map (l (a) (string-append "[" a "]")) indices)))

  (define (c-struct-get a . keys) (string-join (pair (parenthesise-ambiguous a) keys) "."))

  (define (c-struct-pointer-get a . fields)
    (string-append (parenthesise-ambiguous a) (string-join fields "->" (q prefix))))

  (define (c-pointer-get a) (string-append "*" (parenthesise-ambiguous a)))
  (define (c-set name value) (string-append name "=" value))
  (define (c-pointer type) (string-append type " * "))
  (define (c-address-of a) (string-append "&" (parenthesise-ambiguous a)))
  (define (c-not a) (string-append "!" a))
  (define (c-bit-not a) (string-append "~" a))

  (define (c-function-pointer inner type-output . type-input)
    (string-append type-output "(*" inner ")(" (string-join type-input ",") ")"))

  (define c-escape-single-char
    (alist "\"" "\\\"" "\a" "\\a" "\n" "\\n" "\b" "\\b" "\f" "\\f" "\r" "\\r" "\t" "\\t" "\v" "\\v"))

  (define (c-string str)
    (string-enclose
      (fold (l (e r) (string-replace-string r (first e) (tail e))) str c-escape-single-char) "\""))

  (define (c-value a) "handles the default conversions between scheme and c types"
    (cond
      ((symbol? a) (symbol->string a))
      ((string? a) (c-string a))
      ((number? a) (number->string a))
      ((boolean? a) (if a "1" "0"))
      ((char? a) (string-enclose (any->string a) "'"))
      (else (throw (q cannot-convert-to-c-value) a)))))
