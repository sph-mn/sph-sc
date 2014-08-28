(library (sph lang c expressions)
  (export
    c-address-of
    c-apply
    c-apply-nc
    c-bit-shift-nc
    c-compound-nc
    c-convert-type-nc
    c-define
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

  (define (c-stringify arg) (string-append "#" arg))
  (define (cp-undef arg) (string-append "#undef " arg))
  (define (cp-include path) (string-append "#include " (c-string path)))
  (define (cp-includep path) (string-append "#include <" path ">"))
  (define (cp-concat-nc args) (string-join args "##"))

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

  (define (c-compound-nc arg) (string-append "{" arg "}"))
  (define (c-typedef-nc name arg) (c-line-nc "typedef" arg name))

  (define (c-typedef-function-nc name return-type . types)
    (string-append "typedef " (apply c-function-pointer-nc name return-type types)))

  (define (c-convert-type-nc arg type) (string-append "(" type ")(" arg ")"))
  (define (c-line-nc . arg) (string-join arg " "))

  (define* (c-statement-nc keyword body #:optional prefix-arg suffix-arg)
    (string-append keyword (if prefix-arg (parenthesise prefix-arg) "")
      (c-compound-nc body) (if suffix-arg (parenthesise suffix-arg) "")))

  (define (c-enum-nc name enum-list)
    (string-append "enum " name
      (c-compound-nc
        (string-join
          (map (l (ele) (if (list? ele) (string-append (first ele) "=" (first (tail ele))) ele))
            enum-list)
          ","))))

  (define (c-define-nc name type value) "string string -> string"
    (string-append type " " name (if value (string-append "=" value) "")))

  (define* (c-define name type #:optional value) "any [any] -> string"
    (c-define-nc (c-identifier name) (c-identifier type) (if value (c-value value) value)))

  (define (c-identifier arg)
    (string-append
      (cond ((symbol? arg) (symbol->string arg)) ((string? arg) arg)
        (else (raise (q cannot-convert-to-c-identifier))))))

  (define (c-identifier-list arg)
    (parenthesise
      (if (list? arg) (string-join (map c-identifier arg) ",")
        (if (or (symbol? arg) (string? arg)) (c-identifier arg)
          (raise (q cannot-convert-to-c-identifier))))))

  (define (c-identifier+type arg type) (string-append (c-identifier type) " " (c-identifier arg)))

  (define (c-parameter arg types)
    (parenthesise
      (if (list? arg) (string-join (map c-identifier+type arg types) ",")
        (if (or (symbol? arg) (string? arg)) (c-identifier+type arg types)
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

  (define (c-pointer-deref-nc arg key)
    (if key (string-append "*(" arg "+" key ")") (string-append "(*" arg ")")))

  (define (c-bit-shift-nc left? arg shift-by) (string-append arg (if left? "<<" ">>") shift-by))
  (define (c-set-nc name value) (string-append name "=" value))
  (define (c-set name value) (c-set-nc (c-identifier name) (c-value value)))
  (define (c-pointer type) (string-append type " * "))
  (define (c-address-of arg) (string-append "&" arg))

  (define (c-function-pointer-nc name return-type . types)
    (string-append return-type "(*" name ")(" (string-join types ",") ")"))

  (define (c-struct-ref-nc arg key) (string-append arg "." key))
  (define (c-struct-ref arg key) (c-struct-ref-nc (c-identifier arg) (c-identifier key)))

  (define c-escape-single-char
    (alist "\"" "\\\\" "\a" "\\a" "\n" "\\n" "\b" "\\b" "\f" "\\f" "\r" "\\r" "\t" "\\t" "\v" "\\v"))

  (define (c-string str)
    (string-enclose
      (fold (l (ele res) (string-replace-string res (first ele) (tail ele))) str
        c-escape-single-char)
      "\""))

  (define (c-value arg) "handles the default conversions between scheme and c types"
    (cond ((symbol? arg) (symbol->string arg)) ((string? arg) (c-string arg))
      ((number? arg) (number->string arg)) ((boolean? arg) (if arg "1" "0"))
      ((char? arg) (string-enclose (any->string arg) "'")) (else (raise (q cannot-convert-to-c))))))