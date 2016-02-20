(library (sph lang c expressions)
  (export
    c-address-of
    c-apply
    c-apply-nc
    c-bit-shift-nc
    c-compound-nc
    c-convert-type-nc
    c-define
    c-define-array-nc
    c-define-nc
    c-function
    c-function-nc
    c-function-pointer-nc
    c-identifier
    c-if
    c-if-statement
    c-line-nc
    c-parameter
    c-pointer
    c-pointer-deref-nc
    c-set
    c-set-nc
    c-statement-nc
    c-string
    c-stringify
    c-struct-ref
    c-struct-ref-nc
    c-typedef-function-nc
    c-typedef-nc
    c-value
    c-vector
    c-vector-nc
    cp-concat-nc
    cp-define-macro
    cp-define-macro-nc
    cp-if
    cp-include
    cp-includep
    cp-undef
    list->c-vector)
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
  ;by default, the functions convert input values from scheme datatypes,
  ;the variants with a -nc suffix are non-converting and take strings.
  (define (c-stringify a) (string-append "#" a))
  (define (cp-undef a) (string-append "#undef " a))
  (define (cp-include path) (string-append "#include " (c-string path)))
  (define (cp-includep path) (string-append "#include <" path ">"))
  (define (cp-concat-nc a) (string-join a "##"))

  (define (cp-define-macro-nc name body identifier-list)
    (string-append "#define " name (if identifier-list identifier-list "") " " body))

  (define* (cp-define-macro name body #:optional identifier-list)
    (cp-define-macro-nc (c-identifier name) body (map c-identifier identifier-list)))

  (define* (cp-if type test consequent #:optional alternate)
    (string-append "#"
      (if (eq? (q if) type) "if"
        (if (eq? (q ifdef) type) "ifdef"
          (if (eq? (q ifndef) type) "ifndef" (raise (q cannot-convert-to-c)))))
      " " test
      "\n" consequent "\n" (if alternate (string-append "#else\n" alternate "\n") "") "#endif"))

  (define (c-compound-nc a) (string-append "{" a "}"))
  (define (c-typedef-nc name a) (c-line-nc "typedef" a name))

  (define (c-typedef-function-nc name return-type . types)
    (string-append "typedef " (apply c-function-pointer-nc name return-type types)))

  (define (c-convert-type-nc a type) (string-append "(" type ")(" a ")"))
  (define (c-line-nc . a) (string-join a " "))

  (define* (c-statement-nc keyword body #:optional prefix-a suffix-a)
    (string-append keyword (if prefix-a (parenthesise prefix-a) "")
      (c-compound-nc body) (if suffix-a (parenthesise suffix-a) "")))

  (define (c-enum-nc name enum-list)
    (string-append "enum " name
      (c-compound-nc
        (string-join
          (map (l (e) (if (list? e) (string-append (first e) "=" (first (tail e))) e)) enum-list) ","))))

  (define (c-define-array-nc name type size values) "string string string string ... -> string"
    (string-append type " "
      name "[" size "]" (if values (string-append "={" (string-join values ",") "}") "")))

  (define (c-define-nc name type value) "string string -> string"
    (string-append type " " name (if value (string-append "=" value) "")))

  (define* (c-define name type #:optional value) "any [any] -> string"
    (c-define-nc (c-identifier name) (c-identifier type) (if value (c-value value) value)))

  (define (c-identifier a)
    (string-append
      (cond ((symbol? a) (symbol->string a)) ((string? a) a)
        (else (raise (q cannot-convert-to-c-identifier))))))

  (define (c-identifier-list a)
    (parenthesise
      (if (list? a) (string-join (map c-identifier a) ",")
        (if (or (symbol? a) (string? a)) (c-identifier a)
          (raise (q cannot-convert-to-c-identifier))))))

  (define (c-identifier+type a type) (string-append (c-identifier type) " " (c-identifier a)))

  (define (c-parameter a types)
    (parenthesise
      (if (list? a) (string-join (map c-identifier+type a types) ",")
        (if (or (symbol? a) (string? a)) (c-identifier+type a types)
          (raise (q cannot-convert-to-c-parameter))))))

  (define (c-function-nc name type body parameter)
    (string-append type " " name parameter (if body (string-append "{" body "}") "")))

  (define* (c-function name type body #:optional (parameter (list)) (types (list)))
    (c-function-nc (c-identifier name) (c-identifier type) body (c-parameter parameter types)))

  (define* (c-apply-nc proc-name #:optional (args "")) (string-append proc-name "(" args ")"))

  (define* (c-apply proc-name #:optional (args (list)))
    (c-apply-nc (c-identifier proc-name) (string-join (map c-value args) ",")))

  (define* (c-if test consequent #:optional alternate)
    "string string [string] -> string
    create an if expression"
    (string-append "(" test "?" consequent ":" (if alternate alternate "0") ")"))

  (define* (c-if-statement test consequent #:optional alternate)
    "string string [string] -> string
    create an if statement"
    (string-append "if(" test
      "){" consequent "}" (if alternate (string-append "else{" alternate "}") "")))

  (define (c-pointer-deref-nc a key)
    (if key (string-append "*(" a "+" key ")") (string-append "(*" a ")")))

  (define (c-bit-shift-nc left? a shift-by) (string-append a (if left? "<<" ">>") shift-by))
  (define (c-set-nc name value) (string-append name "=" value))
  (define (c-set name value) (c-set-nc (c-identifier name) (c-value value)))
  (define (c-pointer type) (string-append type " * "))
  (define (c-address-of a) (string-append "&" a))

  (define (c-function-pointer-nc name return-type . types)
    (string-append return-type "(*" name ")(" (string-join types ",") ")"))

  (define (c-struct-ref-nc a key) (string-append a "." key))
  (define (c-struct-ref a key) (c-struct-ref-nc (c-identifier a) (c-identifier key)))

  (define c-escape-single-char
    (alist "\"" "\\\\" "\a" "\\a" "\n" "\\n" "\b" "\\b" "\f" "\\f" "\r" "\\r" "\t" "\\t" "\v" "\\v"))

  (define (c-string str)
    (string-enclose
      (fold (l (e r) (string-replace-string r (first e) (tail e))) str c-escape-single-char) "\""))

  (define (c-value a) "handles the default conversions between scheme and c types"
    (cond ((symbol? a) (symbol->string a)) ((string? a) (c-string a))
      ((number? a) (number->string a)) ((boolean? a) (if a "1" "0"))
      ((char? a) (string-enclose (any->string a) "'")) (else (raise (q cannot-convert-to-c))))))