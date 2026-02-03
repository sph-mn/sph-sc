(define-module (sph lang sc))

(use-modules (srfi srfi-1) (srfi srfi-2)
  ((rnrs io ports) #:select (get-datum)) (rnrs eval)
  (ice-9 match) (sph)
  (sph list)
  ( (sph string) #:select
    (string-equal? string-case parenthesize
      parenthesized? any->string
      any->string-write regexp-match-replace regexp-replace string-replace-string string-enclose))
  ( (sph hashtable) #:select
    (ht-create-symbol-q ht-create-symbol ht-delete!
      ht-ref ht-set! ht-from-list ht-hash-symbol ht-create-string))
  ((sph alist) #:select (alist alist-ref))
  ((sph filesystem) #:select (ensure-trailing-slash search-load-path))
  (sph lang sc syntax-extensions))

(export sc->c sc-default-load-paths
  sph-lang-sc-description sc-syntax-table
  sc-call-with-error-printer sc-syntax-check
  sc-syntax-error sc-syntax?
  sc-gensym sc-map-associations
  sc-state-new sc-syntax-error?
  sc-identifier sc-not-preprocessor-keyword?
  sc-not-function-pointer-symbol? sc-path->full-path sc-define-syntax-scm sc-syntax-expand)

(define sph-lang-sc-description
  "an s-expression to c compiler.
   * does syntax checks and displays usage examples on error
   * is parsed using scheme 'read' (scheme comments discarded). the resulting nested list of expressions
     is traversed top to bottom and matching syntax elements are mapped to strings
   * identifier replacements see https://www.gnu.org/software/guile/manual/html_node/API-Overview.html#API-Overview
   * prefixes used in this code file:
     * c-: takes strings or lists of strings and returns strings
     * cp-: like c- but creates code for the c preprocessor
     * sc-: takes sc or strings and returns strings")

(define (vector-accessor index)
  "integer -> procedure:{vector -> any}
   return a procedure that when called with a vector returns the value at index"
  (l (a) (vector-ref a index)))

(define (vector-setter index)
  "integer -> procedure:{vector value -> unspecified}
   return a procedure that when called with a vector and a value sets index to value"
  (l (a value) (vector-set! a index value)))

(define (tree-map-lists f a) "bottom to top"
  (map (l (a) (if (list? a) (f (tree-map-lists f a)) a)) a))

(define (tree-map-lists-top f a) "top to bottom"
  (map (l (a) (if (list? a) (tree-map-lists f (f a)) a)) a))

(define (tree-map-leafs f a) "bottom to top"
  (map (l (a) (if (list? a) (tree-map-leafs f a) (f a))) a))

(define* (tree-contains? a search-value #:optional (equal? equal?))
  "list any [procedure:{any any -> boolean}] -> boolean
   compares all list and non-list elements with search-value and returns true on a match"
  (any (l (a) (or (equal? a search-value) (and (list? a) (tree-contains? a search-value)))) a))

(define* (port->datums port #:optional (get-datum get-datum))
  "string procedure:reader -> list
   read all scheme datums from a port"
  (let loop ((a (get-datum port))) (if (eof-object? a) (list) (pair a (loop (get-datum port))))))

(define* (file->datums path #:optional (get-datum get-datum))
  "string procedure:reader -> list
   read all scheme datums of a file specified by path"
  (call-with-input-file path (l (port) (port->datums port get-datum))))

(define sc-state-load-paths (vector-accessor 1))
(define sc-state-no-semicolon (vector-accessor 2))
(define sc-state-comma-join (vector-accessor 3))
(define sc-state-comma-join-set! (vector-setter 3))
(define sc-state-eval-env (vector-accessor 4))
(define ambiguous-regexp (make-regexp "^(\\*|&)+|\\.|->|\\[|\\("))

(define* (sc-state-new load-paths #:optional eval-env)
  (vector (q sc-state) load-paths
    (ht-create-symbol) #f
    (or eval-env (environment (q (guile)) (q (ice-9 match)) (q (sph lang sc eval-environment))))))

(define (sc-syntax-examples-get name) "prepend the prefix symbol to example argument patterns"
  "symbol -> false/list"
  (and-let* ((examples (ht-ref sc-syntax-examples name)))
    (map (l (a) (if (list? a) (pair name a) a)) examples)))

(define (sc-syntax-error? a) (and (list? a) (not (null? a)) (eq? (q sc-syntax-error) (first a))))

(define* (sc-syntax-error #:optional irritant syntax-name expected)
  "false/any false/symbol false/(any ...) | exception"
  (throw (q sc-syntax-error) irritant
    (or expected (and syntax-name (sc-syntax-examples-get syntax-name)))))

(define* (acount? a min #:optional max) "arity count"
  (let (b (length a)) (and (if min (<= min b) #t) (if max (>= max b) #t))))

(define (sc-association-check state association-count a)
  (= 0 (modulo (length a) association-count)))

(define sc-syntax-examples
  (ht-from-list
    (q
      (address-of ((variable)) case
        ( (predicate subject (match-value/else consequent ...) ..1)
          (= variable (3 (return #t)) (5 (return #t)) (else (return #f))))
        declare ((name type) (name type name type name/type ...) ((name type) ...))
        define
        ( (name type value) (name type value name type value ...)
          ((name parameters ...) (return-type ...) body ...)
          ((name parameters ...) return-type body ...))
        for
        ( ( (init test update) body ...) (((init ...) test (update ...)) body ...)
          (((begin init ...) test (begin update ...)) body ...))
        pointer-get ((variable))
        array-get ((variable indices ...))
        array-set* ((variable values ...))
        convert-type ((variable new-type))
        if ((condition consequent) (condition consequent alternate))
        set ((variable value) (variable value variable value variable/value ...))))
    eq? ht-hash-symbol))

(define (sc-syntax-check-prefix-list prefix a state)
  "symbol list sc-state -> boolean
   arity checks first"
  (case prefix
    ( (sc-define-syntax sc-define-syntax*)
      "dont check contents of syntax definitions as it can contain pattern variables where lists are expected"
      (q ok-terminal))
    ((sc-comment) (and (acount? a 1) (q ok-terminal)))
    ( (+ - *
        / array-get
        cond cond*
        sc-include function-pointer
        label pre-pragma pre-undefine pre-include pre-concat pre-concat-string pre-stringify while)
      (acount? a 1))
    ( (array-set* : struct-pointer-get
        do-while bit-or bit-and bit-xor or and modulo pre-let let let* = < > <= >=)
      (acount? a 2))
    ((array-set struct-set) (acount? a 3))
    ((address-of bit-not goto not pointer-get) (acount? a 1 1))
    ((enum) (acount? a 1 2))
    ((convert-type bit-shift-right bit-shift-left) (acount? a 2 2))
    ((case case*) (match a ((predicate subject clauses ..1) #t) (_ #f)))
    ((declare) (sc-association-check state 2 a))
    ( (define)
      (match a
        ( ( ( (? sc-not-preprocessor-keyword? name) parameter ...)
            ((? sc-not-function-pointer-symbol? return-type) parameter-types ...) body ...)
          (<= (length parameter) (length parameter-types)))
        ((((? sc-not-preprocessor-keyword? name)) return-type body ...) #t)
        ((name type value rest ...) (sc-association-check state 3 rest)) (_ #f)))
    ((for) (match a (((init test update) body ...) #t) (_ #f)))
    ( (if if* pre-if pre-if-defined pre-if-not-defined)
      (match a ((test consequent) #t) ((test consequent alternate) #t) (_ #f)))
    ((pre-define pre-define-if-not-defined) (or (acount? a 1 1) (sc-association-check state 2 a)))
    ((set) (sc-association-check state 2 a))
    ( (struct union)
      (match a (((? symbol?) (name type ...) ...) #t) (((name type ...) ...) #t) (_ #f)))
    (else #t)))

(define (sc-syntax-check a state) "list:expressions (string ...) -> boolean | exception"
  (every
    (l (a)
      (let
        (b
          (if (and (list? a) (not (null? a)))
            (sc-syntax-check-prefix-list (first a) (tail a) state) #t))
        (if (eqv? b (q ok-terminal)) #t
          (if b (if (list? a) (sc-syntax-check a state) #t) (sc-syntax-error a (first a))))))
    a))

(define (sc-call-with-error-printer f)
  (catch (q sc-syntax-error) f
    (l (key irritant expected)
      (simple-format #t "~A\n  irritant: ~S\n  expected any of:\n    ~A\n"
        key irritant (string-join (map (l (a) (simple-format #f "~S" a)) expected) "\n    ")))))

(define (parenthesize-ambiguous a)
  (if (or (parenthesized? a) (not (regexp-exec ambiguous-regexp a))) a (parenthesize a)))

(define (parenthesize-ensure a) (if (parenthesized? a) a (parenthesize a)))
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
        (else (throw (q sc-syntax-error) (q pre-if) type test consequent alternate)))
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

(define (c-for init test update body) (string-append "for(" init ";" test ";" update "){" body "}"))
(define (c-typedef name a) (string-append "typedef " a " " name))

(define (c-convert-type a type)
  "extra round brackets ensure nestability in function pointer cases like this: (dg_pair_reader_t)((*state).reader)(state,count,result)"
  (string-append "((" type ")(" a "))"))

(define* (c-statement keyword body #:optional prefix-a suffix-a)
  "\"keyword (prefix-a) { body } (suffix-a)\""
  (string-append keyword (if prefix-a (parenthesize prefix-a) "")
    (c-compound body) (if suffix-a (parenthesize suffix-a) "")))

(define (c-enum name enum-list)
  (string-append "enum " name
    (c-compound
      (string-join
        (map (l (e) (if (list? e) (string-append (first e) "=" (first (tail e))) e)) enum-list) ","))))

(define (c-define-array name type sizes values) "string string (string ...) string -> string"
  (string-append type " "
    name (apply string-append (map (l (a) (string-append "[" a "]")) sizes))
    (if values (string-append "=" values "") "")))

(define (c-variable name type) "string string -> string" (string-append type " " name))

(define (c-define name type value) "string string [string] -> string"
  (string-append (c-variable name type) (if value (string-append "=" value) "")))

(define (c-identifier a)
  (string-append
    (cond
      ((symbol? a) (symbol->string a))
      ((string? a) a)
      (else (throw (q sc-cannot-convert-to-c-identifier))))))

(define (c-identifier-list a)
  (parenthesize
    (if (list? a) (string-join (map c-identifier a) ",")
      (if (or (symbol? a) (string? a)) (c-identifier a)
        (throw (q sc-cannot-convert-to-c-identifier))))))

(define (c-function-parameter name type) (string-append type " " name))

(define (c-function-parameters names types)
  (parenthesize
    (if (list? names)
      (if (equal? (length names) (length types))
        (string-join (map c-function-parameter names types) ",")
        (throw (q sc-type-and-parameter-list-length-mismatch) names))
      (if (or (symbol? names) (string? names)) (c-function-parameter names types)
        (throw (q sc-cannot-convert-to-c-parameter))))))

(define* (c-function name type-output body #:optional (names (list)) (type-input (list)))
  (string-append (if type-output (string-append type-output " ") "") name
    (catch (q type-and-parameter-list-length-mismatch)
      (nullary (c-function-parameters names type-input)) (l (key . data) (apply throw key name data)))
    (if body (string-append "{" body "}") "")))

(define* (c-apply proc-name #:optional (args ""))
  (string-append (parenthesize-ambiguous proc-name) (parenthesize args)))

(define* (c-if test consequent #:optional alternate)
  "string string [string] -> string
   create an if expression"
  (string-append "(" test "?" consequent ":" (if alternate alternate "0") ")"))

(define* (c-if-statement test consequent #:optional alternate)
  "string string [string] -> string
   create an if statement"
  (string-append "if" (parenthesize-ensure test)
    "{" consequent "}" (if alternate (string-append "else{" alternate "}") "")))

(define (c-array-get a . indices)
  (apply string-append (parenthesize-ambiguous a) (map (l (a) (string-append "[" a "]")) indices)))

(define (c-struct-get a . keys) (string-join (pair (parenthesize-ambiguous a) keys) "."))

(define (c-struct-pointer-get a . fields)
  (string-append (parenthesize-ambiguous a) (string-join fields "->" (q prefix))))

(define (c-pointer-get a) (string-append "*" (parenthesize-ambiguous a)))
(define* (c-set name value #:optional (operator "=")) (string-append name operator value))
(define (c-pointer type) (string-append type " * "))
(define (c-address-of a) (string-append "&" (parenthesize-ambiguous a)))
(define (c-not a) (string-append "!" a))
(define (c-bit-not a) (string-append "~" a))

(define (c-function-pointer inner type-output . type-input)
  (string-append type-output "(*" inner ")(" (string-join type-input ",") ")"))

(define c-escape-single-char
  (alist "\"" "\\\"" "\a" "\\a" "\n" "\\n" "\b" "\\b" "\f" "\\f" "\r" "\\r" "\t" "\\t" "\v" "\\v"))

(define (c-string a)
  (string-enclose
    (fold (l (a result) (string-replace-string result (first a) (tail a))) a c-escape-single-char)
    "\""))

(define (c-value a) "handles the default conversions between scheme and c types"
  (cond
    ((symbol? a) (symbol->string a))
    ((string? a) (c-string a))
    ((number? a) (number->string a))
    ((boolean? a) (if a "1" "0"))
    ( (char? a)
      (let (a (any->string-write (string a)))
        (string-enclose (substring a 1 (- (string-length a) 1)) "'")))
    (else (throw (q sc-cannot-convert-to-c-value) a))))

(define (sc-no-semicolon-register state name)
  "symbol -> unspecified
   dont add semicolon after name when used"
  (ht-set! (sc-state-no-semicolon state) name #t))

(define (sc-no-semicolon-unregister state name) (ht-delete! (sc-state-no-semicolon state) name))
(define (sc-no-semicolon-registered? state name) (ht-ref (sc-state-no-semicolon state) name))

(define (sc-no-semicolon expressions compile state)
  "(sc ...) -> c
   dont add semicolons after expressions. for macros that expand to function definitions whose body must not
   end with a semicolon according to iso c11"
  (string-join (map compile expressions) "\n" (q suffix)))

(define (sc-no-semicolon-track state macro-name a)
  "iso c11 does not allow semicolons at the end of function bodies like {};
   because sc can not know from macro application syntax if it expands to a function definition,
   it needs to track all macro definitions to decide if a semicolon needs to be added.
   this does not work when using macro definitions from preprocessor included c code -
   in this case only sc-no-semicolon expressions, sc-insert or a compiler option (enabled by default in gcc) can solve this"
  (match a (((quote begin) initial ... last) (sc-no-semicolon-track state macro-name last))
    (((quote define) (name parameters ...) body ...) (sc-no-semicolon-register state macro-name))
    (else a)))

(define-syntax-rule (add-begin-if-multiple a) (if (= 1 (length a)) (first a) (pair (q begin) a)))
(define (contains-set? a) "list -> boolean" (and (list? a) (tree-contains? a (q set))))
(define (preprocessor-keyword? a) (and (symbol? a) (string-prefix? "pre-" (symbol->string a))))
(define sc-not-preprocessor-keyword? (negate preprocessor-keyword?))
(define (sc-not-function-pointer-symbol? a) (not (and (symbol? a) (eq? (q function-pointer) a))))
(define (docstring->comment a) (string-append "\n/** " a " */\n"))

(define (add-begin a)
  (if (and (list? a) (not (null? a)) (equal? (q begin) (first a))) a (list (q begin) a)))

(define (alist->regexp-match-replacements a)
  "converts strings alist key strings to regular expressions"
  (map (l (e) (pair (if (string? (first e)) (make-regexp (first e)) (first e)) (tail e))) a))

(define identifier-replacements
  (alist->regexp-match-replacements
    (alist "->" "_to_"
      ".-" (pair "-" "_")
      "-." (pair "-" "_")
      ".!" (pair "!" "_x")
      "\\?" "_p"
      "./." (pair "/" "_or_")
      ".<" (pair "<" "_less")
      ".>" (pair ">" "_gr")
      ".<=" (pair "<=" "_leq") ".>=" (pair ">=" "_geq") ".%" (pair "%" "_percent"))))

(define sc-identifier-prefixes (list #\& #\*))
(define sc-identifier-infixes (list #\. #\:))

(define (sc-identifier-struct-pointer-get a)
  (let (a (string-split a #\:)) (if (= 1 (length a)) (first a) (apply c-struct-pointer-get a))))

(define (translate-identifier a)
  (let
    ( (a (regexp-match-replace a identifier-replacements))
      (contains-infix (any (l (char) (string-index a char)) sc-identifier-infixes))
      (after-prefix-index (string-skip a (l (a) (containsq? sc-identifier-prefixes a)))))
    (if (and after-prefix-index (not (zero? after-prefix-index)))
      (if contains-infix
        (string-append (substring a 0 after-prefix-index)
          (parenthesize (sc-identifier-struct-pointer-get (substring a after-prefix-index))))
        a)
      (if contains-infix (sc-identifier-struct-pointer-get a) a))))

(define (sc-identifier a)
  (if (symbol? a) (translate-identifier (symbol->string a))
    (if (list? a) (string-join (map sc-identifier a) " ") (any->string a))))

(define (sc-let* a compile state)
  (c-compound
    (match a
      ( ( ( (names values ...) ...) body ...)
        (compile
          (pair (q begin)
            (append
              (map (l (n v) (pairs (if (= 1 (length v)) (q set) (q define)) n v)) names values) body)))))))

(define (sc-map-associations association-count f a) (map-slice association-count f a))

(define (sc-pre-define-if-not-defined a compile state)
  (compile
    (pair (q begin)
      (sc-map-associations 2
        (l (name value)
          (let
            (identifier (match name (((? sc-not-preprocessor-keyword? name) _ ...) name) (_ name)))
            (qq
              (pre-if-not-defined (unquote identifier)
                (unquote
                  (if value (qq (pre-define (unquote name) (unquote value)))
                    (qq (pre-define (unquote name)))))))))
        (if (= 1 (length a)) (list (first a) #f) a)))))

(define (sc-do-while a compile state)
  (match a
    ( (test body ...)
      (string-append "do" (c-compound (compile (pair (q begin) body)))
        "while" (parenthesize (compile test))))))

(define (sc-pre-let* a compile state)
  (match a
    ( ( (names+values ...) body ...)
      (string-replace-string
        (string-append
          (string-join
            (sc-map-associations 2 (l (n v) (compile (list (q pre-define) n v))) names+values) "\n"
            (q suffix))
          (compile (pair (q begin) body)) "\n"
          (string-join
            (sc-map-associations 2
              (l (n v) (compile (list (q pre-undefine) (if (pair? n) (first n) n)))) names+values)
            "\n"))
        "\n\n" "\n"))
    (_ (throw (q sc-syntax-error-for-pre-let*)))))

(define (sc-for a compile state)
  (let
    (comma-join
      (l (a)
        (match a (((quote begin) a ...) (string-join (map compile a) ","))
          ( ( (? symbol?) _ ...) (sc-state-comma-join-set! state #t)
            (let (result (compile a)) (sc-state-comma-join-set! state #f) result))
          (_ (string-join (map compile a) ",")))))
    (match a
      ( ( (init test update) body ...)
        (c-for (comma-join init) (compile test) (comma-join update) (compile (pair (q begin) body)))))))

(define (sc-if a compile state)
  (match a
    ( (test consequent alternate)
      (c-if-statement (compile test) (compile (list (q begin) consequent))
        (compile (list (q begin) alternate))))
    ((test consequent) (c-if-statement (compile test) (compile (list (q begin) consequent))))))

(define (sc-semicolon-list-to-comma-list a)
  "this would not work with string or character literals that contain semicolons.
   if these literals can occur anywhere other than definitions this needs to be changed"
  (parenthesize (string-replace-string (string-trim-right a #\;) ";" ",")))

(define (sc-if* a compile state)
  (apply c-if
    (map
      (l (b)
        (match b
          ( ( (quote begin) body ...)
            (parenthesize
              (string-join
                (map
                  (l (b)
                    (if (contains-set? b) (sc-semicolon-list-to-comma-list (compile b)) (compile b)))
                  body)
                ",")))
          (_ (if (contains-set? b) (sc-semicolon-list-to-comma-list (compile b)) (compile b)))))
      a)))

(define (sc-apply name a compile state)
  (string-append
    (c-apply (compile name) (string-join (map (compose parenthesize-ambiguous compile) a) ","))
    (if (sc-no-semicolon-registered? state name) "\n" "")))

(define (sc-join-expressions a) "main procedure for the concatenation of toplevel expressions"
  (define (fold-f b prev)
    (pair
      (cond
        ( (string-prefix? "#" b) "preprocessor directives need to be on a separate line"
          (if (and (not (null? prev)) (string-suffix? "\n" (first prev))) (string-append b "\n")
            (string-append "\n" b "\n")))
        ( (or (string-prefix? "/*" b) (string-prefix? "//" b))
          (if (and (not (null? prev)) (string-suffix? "\n" (first prev))) b (string-append "\n" b)))
        ((or (string-suffix? "\n" b) (string-suffix? ";" b)) b)
        ((string-suffix? ":" b) (string-append b "\n"))
        (else (string-append b ";")))
      prev))
  (apply string-append (reverse (fold fold-f null (remove string-null? a)))))

(define* (sc-define-type compile name value) "any any -> string"
  (let (name (compile name))
    (if (sc-function-pointer? value)
      (string-append "typedef " (apply sc-function-pointer compile name (tail value)))
      (c-typedef name (compile value)))))

(define (sc-compile-type a compile)
  (if (list? a)
    (if
      (or (null? a)
        (let (a-first (first a))
          (not
            (and (symbol? a-first)
              (or (preprocessor-keyword? a-first) (eq? (q function-pointer) a-first)
                (eq? (q array) a-first) (eq? (q sc-insert) a-first))))))
      (string-join (map compile a) " ") (compile a))
    (sc-identifier a)))

(define (sc-compile-types a compile)
  (parenthesize (string-join (map (l (e) (sc-compile-type e compile)) a) ",")))

(define (sc-function-pointer? a)
  (and (list? a) (not (null? a)) (eq? (q function-pointer) (first a))))

(define (sc-array? a) (and (list? a) (not (null? a)) (eq? (q array) (first a))))

(define (sc-function-pointer compile inner type-output . type-input)
  "procedure string ? ? ... -> string"
  (if (sc-function-pointer? type-output)
    (apply sc-function-pointer compile
      (string-append (parenthesize (string-append "*" inner)) (sc-compile-types type-input compile))
      (tail type-output))
    (string-append (sc-compile-type type-output compile) (parenthesize (string-append "*" inner))
      (sc-compile-types type-input compile))))

(define (get-body-and-docstring& body compile macro-function? c)
  "list procedure:{string:docstring string:body -> any} -> any"
  (if (null? body) (c #f #f)
    (apply
      (l (docstring body)
        (c docstring
          (if macro-function? (string-trim-right (sc-join-expressions (map compile body)) #\;)
            (if (null? body) #f (sc-join-expressions (map compile body))))))
      (if macro-function?
        (match (first body)
          ( ( (quote begin) (? string? docstring) body ...)
            (list (docstring->comment docstring) body))
          (_ (list #f body)))
        (if (string? (first body)) (list (docstring->comment (first body)) (tail body))
          (list #f body))))))

(define (pad-parameter-names a b)
  (let* ((a-length (length a)) (b-length (length b)) (difference (- b-length a-length)))
    (if (> difference 0) (append a (make-list difference #f)) a)))

(define (sc-function compile name return-type body parameter-names parameter-types)
  (let*
    ( (return-type (if (and (null? return-type) (null? body)) (q (void)) return-type))
      (parameter-types (if (null? parameter-types) (q (void)) parameter-types))
      (parameters
        (sc-function-parameters compile (pad-parameter-names parameter-names parameter-types)
          parameter-types name))
      (name (compile name)))
    (get-body-and-docstring& body compile
      #f
      (l (docstring body-string)
        (let (body-string (if body-string (c-curly-brackets body-string) ""))
          (string-append (or docstring "")
            (if (sc-function-pointer? return-type)
              (string-append
                (apply sc-function-pointer compile
                  (string-append name parameters) (tail return-type))
                body-string)
              (string-append (sc-compile-type return-type compile) " " name parameters body-string))
            (if (string-null? body-string) "" "\n")))))))

(define (sc-macro-function name parameter body compile)
  (get-body-and-docstring& body compile
    #t
    (l (docstring body-string)
      (string-append (or docstring "")
        (string-replace-string
          (string-trim-right
            (cp-define (sc-identifier name) body-string (sc-identifier-list parameter)) #\newline)
          "\n" "\\\n")
        (if docstring "\n" "")))))

(define (ref-sc-function-pointer compile inner type-output . type-input)
  "procedure string ? ? ... -> string"
  (if (sc-function-pointer? type-output)
    (apply sc-function-pointer compile
      (string-append (parenthesize (string-append "*" inner)) (sc-compile-types type-input compile))
      (tail type-output))
    (string-append (sc-compile-type type-output compile) (parenthesize (string-append "*" inner))
      (sc-compile-types type-input compile))))

(define (sc-array compile name type) "the implementation is incomplete"
  (match type
    ( (type size ...)
      (string-append (sc-compile-type type compile) " " (apply c-array-get name (map compile size))))))

(define (sc-function-parameter compile name type)
  (cond
    ( (sc-function-pointer? type)
      (apply sc-function-pointer compile (if name (compile name) "") (tail type)))
    ((sc-array? type) (sc-array compile (if name (compile name) "") (tail type)))
    (else
      (string-append (sc-compile-type type compile) (if name (string-append " " (compile name)) "")))))

(define (sc-function-parameters compile names types function-name)
  (parenthesize
    (if (list? names)
      (string-join (map (l a (apply sc-function-parameter compile a)) names types) ",")
      (if (or (symbol? names) (string? names)) (string-append (compile types) " " (compile names))
        (throw (q sc-cannot-convert-to-c-parameter))))))

(define (sc-identifier-list a) (parenthesize (string-join (map sc-identifier a) ",")))

(define (sc-value a)
  (cond
    ((symbol? a) (translate-identifier (symbol->string a)))
    ((boolean? a) (if a "1" "0"))
    (else (c-value a))))

(define (sc-pre-if type a compile)
  (match a
    ( (test consequent alternate)
      (cp-if type (compile test) (compile (add-begin consequent)) (compile (add-begin alternate))))
    ((test consequent) (cp-if type (compile test) (compile (add-begin consequent)) #f))))

(define (sc-pre-define a compile state) "-> string"
  (match a ((name) (cp-define (sc-identifier name)))
    ( (name value)
      (match name
        ( (name parameter ...) (sc-no-semicolon-track state name value)
          (sc-macro-function name parameter (list value) compile))
        (_ (sc-no-semicolon-track state name value)
          (cp-define (sc-identifier name)
            (string-replace-string (string-trim-right (compile value) #\newline) "\n" "\\\n")))))
    ( (name-1 value-1 name-2 value-2 rest ...)
      (compile (pair (q begin) (sc-map-associations 2 (l a (pair (q pre-define) a)) a))))
    (_ #f)))

(define (sc-pre-include paths)
  "(string ...) -> string
   uses c load path <> notation when a given path does not start with a slash or with a directory reference"
  (string-join
    (map
      (l (a)
        ( (if (or (string-prefix? "./" a) (string-prefix? "../" a) (string-prefix? "/" a))
            cp-include-path cp-include)
          a))
      paths)
    "\n"))

(define (sc-pre-include-variable name)
  "symbol -> symbol
   return a name for a preprocessor variable marking a file as having been included.
   #define sc_included_{name}"
  (string->symbol (string-append "sc_included_" (symbol->string name))))

(define (sc-pre-include-define name)
  "symbol -> string
   define a preprocessor variable for marking a file as having been included"
  (list (q pre-define) (sc-pre-include-variable name)))

(define (sc-enum-entries a) "list -> string"
  (string-join
    (map
      (l (e)
        (match e ((name value) (string-append (sc-identifier name) "=" (sc-value value)))
          (name (sc-identifier name))))
      a)
    ","))

(define (sc-enum a compile state)
  (let (c (l (name entries) (c-statement (string-append "enum" name) (sc-enum-entries entries))))
    (match a ((name (entries ...)) (c (string-append " " (sc-identifier name)) entries))
      (((entries ...)) (c "" entries)))))

(define sc-included-paths (ht-create-string))

(define (sc-path->full-path load-paths path) "expects load paths to have a trailing slash"
  (let*
    ( (path (string-append path ".sc"))
      (path-found
        (if (string-prefix? "/" path) (and (file-exists? path) path)
          (search-load-path path load-paths))))
    (if path-found (canonicalize-path path-found)
      (throw (q sc-file-not-accessible)
        (string-append (any->string path) " not found in " (any->string load-paths))))))

(define (sc-include-sc paths compile state) "(string ...) (string ...) -> list"
  (pair (q begin)
    (append-map
      (l (a) (let (a (sc-path->full-path (sc-state-load-paths state) a)) (file->datums a read)))
      paths)))

(define (sc-include-sc-once paths compile state) "(string ...) (symbol/string ...) -> list"
  (pair (q begin)
    (apply append
      (map
        (l (path)
          (let (path (sc-path->full-path (sc-state-load-paths state) path))
            (if (ht-ref sc-included-paths path) null
              (begin (ht-set! sc-included-paths path #t) (file->datums path)))))
        paths))))

(define (sc-list? a)
  (and (list? a) (or (null? a) (not (and (symbol? (first a)) (sc-syntax? (first a)))))))

(define (sc-array-literal a compile state) "list -> string"
  (string-append "{"
    (string-join (map (l (a) (if (sc-list? a) (sc-array-literal a compile state) (compile a))) a)
      ",")
    "}"))

(define (sc-define-array a compile state)
  (match a
    ( (name type size values ...)
      (let (size (any->list size))
        (c-define-array (compile name)
          (match type (((? preprocessor-keyword? _) _ ...) (compile type))
            (else (sc-identifier type)))
          (if (null? size) (list "") (map (l (a) (if (null? a) "" (compile a))) size))
          (if (null? values) #f (sc-array-literal values compile state)))))))

(define (sc-define a compile state)
  "(argument ...) procedure -> string/false
   if the first argument is a preprocessor command, it is a variable declaration.
   it is still possible to construct function names with the preprocessor"
  (match a
    ( ( ( (? sc-not-preprocessor-keyword? name) parameter ...)
        ((? sc-not-function-pointer-symbol? return-type) types ...) body ...)
      (sc-function compile name return-type body parameter types))
    ( ( ( (? sc-not-preprocessor-keyword? name)) return-type body ...)
      (sc-function compile name return-type body null null))
    ( (name type value rest ...)
      (string-join
        (sc-map-associations 3
          (l (id type value)
            (match type
              ( ( (quote struct-variable) type a ...)
                (sc-define (list id type (pair (q struct-literal) a)) compile state))
              ( ( (quote array) type size)
                (string-append (sc-define-array (list id type size) compile state) "="
                  (compile value)))
              (_ (c-define (compile id) (sc-compile-type type compile) (compile value)))))
          a)
        ";"))
    (_ #f)))

(define (sc-syntax? a) (if (ht-ref sc-syntax-table a) #t #f))

(define sc-gensym
  (let (gensym-counter 0)
    (nullary
      "return a quasi-unique identifier (not returned twice but not checked for if user defined). can be used inside macros"
      (set! gensym-counter (+ 1 gensym-counter))
      (string->symbol (string-append "_t" (number->string gensym-counter 32))))))

(define (sc-struct-type? x)
  (and (list? x) (>= (length x) 2)
    (let ((kw (first x)) (id (second x)))
      (and (or (eq? kw (quote struct)) (eq? kw (quote union)))
        (or (symbol? id) (and (list? id) (preprocessor-keyword? (first id)))) (null? (cddr x))))))

(define (sc-struct-or-union-body a compile state)
  (string-join
    (map
      (l (a)
        (match a
          ( (name (? sc-struct-type? t))
            (string-append (sc-compile-type t compile) " " (compile name)))
          ( (name ((or (quote struct) (quote union)) hd ...))
            (let ((keyword (first (second a))))
              (string-append (symbol->string keyword) " {"
                (sc-struct-or-union-body (cdr (second a)) compile state) "} " (compile name))))
          ( ( (or (quote struct) (quote union)) body ...)
            (string-append (symbol->string (first a)) " "
              (string-append "{" (sc-struct-or-union-body body compile state) "}")))
          ( (name ((or (quote struct) (quote union)) body ...))
            (let ((keyword (first (second a))))
              (string-append (symbol->string keyword) " {"
                (sc-struct-or-union-body body compile state) "} " (compile name))))
          ( (name type (? integer? bits))
            (string-append
              (string-join (map (l (a) (sc-compile-type a compile)) type) " " (q suffix))
              (compile name) ":" (sc-value bits)))
          ( (name type)
            (if (sc-function-pointer? type)
              (apply sc-function-pointer compile (compile name) (tail type))
              (match type (((quote array) a ...) (sc-define-array (pair name a) compile state))
                (else (string-append (sc-compile-type type compile) " " (compile name))))))))
      a)
    ";" (q suffix)))

(define (sc-struct-or-union keyword a compile state) "symbol/false ? procedure -> string"
  (let
    ( (keyword-string (symbol->string keyword)) (a-first (first a))
      (c (l (name body) (c-statement name (sc-struct-or-union-body body compile state)))))
    (if (or (symbol? a-first) (and (list? a-first) (preprocessor-keyword? (first a-first))))
      (c (string-append keyword-string " " (compile a-first)) (tail a)) (c keyword-string a))))

(define (sc-declare-variable name type compile)
  (if (sc-function-pointer? type) (apply sc-function-pointer compile (compile name) (tail type))
    (c-variable (compile name) (sc-compile-type type compile))))

(define (sc-struct-literal a compile state)
  (c-compound (map (l (a) (if (list? a) (map compile a) (compile a))) a)))

(define (sc-declare a compile state)
  (sc-join-expressions
    (sc-map-associations 2
      (l (id type)
        (match type
          ( ( (quote struct-variable) type a ...)
            (sc-define (list id type (pair (q struct-literal) a)) compile state))
          ( ( (quote array) type size values ...)
            (sc-define-array (pairs id type size values) compile state))
          (((quote array) type size) (sc-define-array (list id type size) compile state))
          (((quote enum) a ...) (sc-enum a compile state))
          ( ( (or (quote struct) (quote union)) (not (? symbol?)) _ ...)
            (sc-struct-or-union (first type) (pair id (tail type)) compile state))
          (((quote type) type) (sc-define-type compile id type))
          (_ (or (sc-define (list id type) compile state) (sc-declare-variable id type compile)))))
      a)))

(define (sc-set-join state a)
  (if (null? (tail a)) (first a)
    (if (sc-state-comma-join state) (string-join a ",") (sc-join-expressions a))))

(define* (sc-set-f operator)
  (l (a compile state)
    (sc-set-join state
      (sc-map-associations 2 (l (name value) (c-set (compile name) (compile value) operator)) a))))

(define (sc-cond* a compile state)
  (compile
    (let (conditions (reverse a))
      (fold
        (l (condition alternate)
          (list (q if*) (first condition) (add-begin-if-multiple (tail condition)) alternate))
        (match (first conditions) (((quote else) body ...) (add-begin-if-multiple body))
          ((test consequent ...) (list (q if*) test (add-begin-if-multiple consequent))))
        (tail conditions)))))

(define* (sc-pre-cond-if if-type add-endif test consequent #:optional alternate)
  (string-replace-string
    (string-append "#"
      (case if-type ((elif) "elif") ((if) "if") ((ifdef) "ifdef") ((ifndef) "ifndef")) " "
      test "\n"
      consequent "\n"
      (if alternate (string-append "#else\n" alternate "\n") "") (if add-endif "#endif" ""))
    "\n\n" "\n"))

(define (sc-pre-cond if-type a compile)
  (match a
    ( (only-cond)
      (sc-pre-cond-if if-type #t
        (compile (first only-cond)) (compile (pair (q begin) (tail only-cond)))))
    ( (first-cond middle-cond ... last-cond)
      (string-append
        (sc-pre-cond-if if-type #f
          (compile (first first-cond)) (compile (pair (q begin) (tail first-cond))))
        (apply string-append
          (map
            (l (a)
              (sc-pre-cond-if (q elif) #f (compile (first a)) (compile (pair (q begin) (tail a)))))
            middle-cond))
        (let
          ( (test (compile (first last-cond)))
            (consequent (compile (pair (q begin) (tail last-cond)))))
          (if (eq? (q else) (first last-cond)) (string-append "#else\n" consequent "\n#endif")
            (sc-pre-cond-if (q elif) #t test consequent)))))))

(define (sc-cond a compile state)
  (match a
    ( (only-cond)
      (c-if-statement (compile (first only-cond)) (compile (pair (q begin) (tail only-cond)))))
    ( (first-cond middle-cond ... last-cond)
      (string-append
        (c-if-statement (compile (first first-cond)) (compile (pair (q begin) (tail first-cond))))
        (apply string-append
          (map
            (l (a)
              (string-append "else "
                (c-if-statement (compile (first a)) (compile (pair (q begin) (tail a))))))
            middle-cond))
        (let
          ( (test (compile (first last-cond)))
            (consequent (compile (pair (q begin) (tail last-cond)))))
          (string-append "else"
            (if (eq? (q else) (first last-cond)) (string-append "{" consequent "}")
              (string-append " " (c-if-statement test consequent)))))))))

(define (sc-while a compile state)
  (match a
    ( (test body ...)
      (string-append "while" (parenthesize (compile test))
        (c-compound (compile (pair (q begin) body)))))))

(define (sc-default-load-paths)
  (map ensure-trailing-slash (let (a (getenv "SC_LOAD_PATH")) (if a (string-split a #\:) null))))

(define (sc-array-set a compile state)
  (let (array (first a))
    (compile
      (pair (q begin)
        (sc-map-associations 2
          (l (index value) (list (q set) (list (q array-get) array index) value)) (tail a))))))

(define (sc-array-set* a compile state)
  (let (array (first a))
    (compile
      (pair (q begin)
        (map-with-index (l (index value) (list (q set) (list (q array-get) array index) value))
          (tail a))))))

(define (sc-case-f is-case*)
  (l (a compile state)
    (compile
      (match a
        ( (predicate subject clauses ..1)
          (pair (if is-case* (q cond*) (q cond))
            (map
              (l (a)
                (match a (((quote else) _ ...) a)
                  ( ( (objects ...) body ...)
                    (pair (pair (q or) (map (l (b) (list predicate b subject)) objects)) body))
                  ((object body ...) (pair (list predicate object subject) body))))
              clauses)))))))

(define (sc-struct-set-f getter) "symbol:struct-get/struct-pointer-get/... -> procedure"
  (l (a compile state)
    (let (struct (first a))
      (compile
        (pair (q begin)
          (sc-map-associations 2 (l (field value) (list (q set) (list getter struct field) value))
            (tail a)))))))

(define* (sc-infix-f c-infix #:optional can-be-prefix)
  (l (a compile state)
    (let
      (content
        (map
          (l (a) "consider cases like a&&b=c where a lvalue error would occur for b=c"
            (if (contains-set? a) (parenthesize (compile a)) (compile a)))
          a))
      (parenthesize
        (if (= 1 (length a))
          (string-case c-infix ("/" (apply string-append "1" c-infix content))
            (("+" "-") (apply string-append c-infix content)))
          (string-join content c-infix))))))

(define (sc-comparison-infix-f c-infix)
  (l (a compile state)
    (let (operator (if (eq? "=" c-infix) "==" c-infix))
      ( (if (= 2 (length a)) identity parenthesize)
        (string-join
          (map-segments 2 (l (a b) (parenthesize (string-append a operator b)))
            (map (l (a) (if (contains-set? a) (parenthesize (compile a)) (compile a))) a))
          "&&")))))

(define (sc-address-of a compile state) (c-address-of (apply string-append (map compile a))))

(define (match->alist a pattern eval-environment)
  "matches with (ice-9 match) but takes pattern as a variable and returns
   matches in an association list ((pattern-identifier . value) ...)"
  (let (ids (delete (quote ...) (flatten pattern)))
    (eval
      (qq
        (match (unquote (list (q quote) a))
          ( (unquote pattern)
            (list (unquote-splicing (map (l (id) (list (q cons) (list (q quote) id) id)) ids))))))
      eval-environment)))

(define (get-ellipsis-ids a)
  "return a list of all identifiers followed by ..., which ice-9 match will always put into a list,
   to differentiate from lists matched by non-ellipsis variables"
  (if (null? a) a
    (let loop ((a (first a)) (rest (tail a)))
      (if (null? rest) null
        (if (eq? (q ...) (first rest))
          (append (flatten (any->list a))
            (if (null? (tail rest)) null (loop (first (tail rest)) (tail (tail rest)))))
          (if (list? a) (append (get-ellipsis-ids a) (loop (first rest) (tail rest)))
            (loop (first rest) (tail rest))))))))

(define (get-matches a pattern eval-environment c) "c:{repeated-ids singular-ids -> any}"
  (let
    ((matches (match->alist a pattern eval-environment)) (ellipsis-ids (get-ellipsis-ids pattern)))
    (apply-values c (partition (l (a) (containsq? ellipsis-ids (first a))) matches))))

(define (replace-pattern a replacements)
  "any:pattern alist -> (pattern ...)
   receives a pattern that is followed by an ellipsis and replaces placeholders and repeats the pattern for each available value.
   example
     pattern: (a), replacements: ((a 1 2)), result: ((1) (2)).
   the placeholder with the most values defines the count of repetition, the last value is repeated for missing values.
   pattern: (a b), replacements: ((a 1 2) (b 3)), result: ((1 3) (2 3))
   matches are replaced with vectors in an intermediate step to support nested ellipsis ((name data ...) ...)"
  (let*
    ( (replacements
        (filter-map
          (l (a) "add vector placeholders"
            (if (symbol? a) (assq a replacements)
              (if (vector? a) (pair (vector-ref a 0) (vector-ref a 1)) #f)))
          (flatten (list a))))
      (repetition (apply max (map (l (a) (if (list? (tail a)) (length (tail a)) 1)) replacements)))
      (replacements
        (map
          (l (a) "normalize lengths and repeat the last value if shorter"
            (let (values (any->list (tail a)))
              (pair (first a)
                (if (> repetition (length values))
                  (append values (make-list (- repetition (length values)) (last values))) values))))
          replacements)))
    (apply append
      (map-integers repetition
        (l (index) "replace all placeholders in pattern and repeat for each value"
          (tree-map-leafs
            (l (a)
              (let
                (values
                  (if (vector? a) (alist-ref replacements (vector-ref a 0))
                    (if (symbol? a) (alist-ref replacements a) #f)))
                (if values (vector a (list-ref values index)) a)))
            (list a)))))))

(define (replace-ellipsis-one a replacements) "expand all patterns followed by ... in list"
  (if (null? a) a
    (let loop ((a (first a)) (rest (tail a)))
      (if (null? rest) (list a)
        (if (eq? (q ...) (first rest))
          (append (replace-pattern a replacements)
            (if (null? (tail rest)) null (loop (first (tail rest)) (tail (tail rest)))))
          (pair a (loop (first rest) (tail rest))))))))

(define (replace-identifiers a replacements)
  (tree-map-leafs
    (l (a) (let (key-value (assoc a replacements)) (if key-value (tail key-value) a))) a))

(define (replace-ellipsis a replacements)
  "expand all patterns followed by ... in lists and sublists"
  (tree-map-leafs (l (a) (if (vector? a) (vector-ref a 1) a))
    (tree-map-lists-top (l (a) (replace-ellipsis-one a replacements)) (list a))))

(define (sc-define-syntax-scm id pattern expansion)
  "define new sc syntax from scheme.
   in scheme:
     (sc-define-syntax-scm test (quote (a b ...)) (quote (a (+ 1 b) ...)))
   or
     (sc-define-syntax-scm test (quote (a b ...)) (lambda (state compile a b) (cons* (q printf) \"%d %d\" a b)))"
  (if (procedure? expansion)
    (ht-set! sc-syntax-table id
      (l (a compile state)
        (let (replacements (match->alist a pattern (sc-state-eval-env state)))
          (apply expansion state compile (map tail replacements)))))
    (ht-set! sc-syntax-table id
      (l (a compile state)
        (get-matches a pattern
          (sc-state-eval-env state)
          (l (repeated single)
            "replace-ellipsis might replace in expanded code if the expanded code contains ellipsis
             this is currently handled by replacing non-repeated identifiers first"
            (first (replace-ellipsis (replace-identifiers expansion single) repeated))))))))

(define (sc-define-syntax a compile state)
  "define new syntax in sc using syntax-rules style pattern matching.
   non-hygienic, that means, without protecting against conflicts with identifiers in the surrounding environment.
   examples:
     (define-syntax (test (a b) ...) ((+ a b) ...))"
  (match a (((id pattern ...) expansion) (sc-define-syntax-scm id pattern expansion))
    (((id pattern ...) (? string? docstring) expansion) (sc-define-syntax-scm id pattern expansion)))
  "")

(define (sc-define-syntax* a compile state)
  "define new syntax in sc using a scheme expression. non-hygienic.
   the scheme expression can return sc as a list or c as a string.
   in sc:
     (define-syntax* (test a b ...) (cons* (q printf) \"%d %d\" a b))"
  (apply
    (l (id pattern scheme-expression)
      (let (formals (pairs (q sc-state) (q sc-compile) (delete (q ...) (flatten pattern))))
        (sc-define-syntax-scm id pattern
          (eval (list (q lambda) formals scheme-expression) (sc-state-eval-env state)))))
    (match a (((id pattern ...) scheme-expression) (list id pattern scheme-expression))
      (((id pattern ...) docstring scheme-expression) (list id pattern scheme-expression))))
  "")

(define (sc-syntax-expand id pattern) "return the direct result from a syntax handler"
  (let (state (sc-state-new (sc-default-load-paths)))
    ((ht-ref sc-syntax-table id) pattern (l (a) (sc->c* a state)) state)))

(define (sc-comment a c s)
  "note: // comments dont work inside preprocessor macros,
   because the \\ to escape the newline will make it apply to
   the rest of the preprocessor macro"
  (let (a-string (string-join (map any->string a) "\n"))
    (if (string-contains a-string "\n") (string-append "/* " a-string " */\n")
      (string-append "\n/* " a-string " */\n"))))

(define (sc-type-identifier a compile)
  (if
    (and (list? a) (or (containsq? (q (struct union enum)) (first a)) (not (sc-syntax? (first a)))))
    (string-join (map compile a) " ") (compile a)))

(define sc-syntax-table
  (ht-create-symbol-q : (l (a c s) (apply c-struct-pointer-get (map c a)))
    != (sc-comparison-infix-f "!=")
    < (sc-comparison-infix-f "<")
    <= (sc-comparison-infix-f "<=")
    = (sc-comparison-infix-f "=")
    > (sc-comparison-infix-f ">")
    >= (sc-comparison-infix-f ">=")
    * (sc-infix-f "*")
    + (sc-infix-f "+" #t)
    - (sc-infix-f "-" #t)
    / (sc-infix-f "/")
    address-of sc-address-of
    and (sc-infix-f "&&")
    array-get (l (a compile state) (apply c-array-get (map compile a)))
    array-literal sc-array-literal
    array-set sc-array-set
    array-set* sc-array-set*
    begin (l (a compile state) (sc-join-expressions (map compile a)))
    bit-and (sc-infix-f "&")
    bit-not (l (a compile state) (c-bit-not (compile (first a))))
    bit-or (sc-infix-f "|")
    bit-shift-left (sc-infix-f "<<")
    bit-shift-right (sc-infix-f ">>")
    bit-xor (sc-infix-f "^")
    case (sc-case-f #f)
    case* (sc-case-f #t)
    compound-statement (l (a compile state) (c-compound (sc-join-expressions (map compile a))))
    cond sc-cond
    cond* sc-cond*
    convert-type
    (l (a compile state)
      (c-convert-type (compile (first a)) (sc-type-identifier (second a) compile)))
    declare sc-declare
    define sc-define
    do-while sc-do-while
    enum sc-enum
    for sc-for
    function-pointer (l (a compile state) (apply sc-function-pointer compile "" a))
    array (l (a compile state) (apply sc-array compile a))
    goto (l (a compile state) (string-append "goto " (compile (first a))))
    if sc-if
    if* sc-if*
    label
    (l (a compile state)
      (string-append (compile (first a)) ":" (sc-join-expressions (map compile (tail a)))))
    let* sc-let*
    modulo (sc-infix-f "%")
    not (l (a compile state) (c-not (compile (first a))))
    or (sc-infix-f "||")
    pointer-get (l (a compile state) (apply c-pointer-get (map compile a)))
    pre-concat (l (a compile state) (cp-concat (map sc-identifier a)))
    pre-cond-defined (l (a c s) (sc-pre-cond (q ifdef) a c))
    pre-cond (l (a c s) (sc-pre-cond (q if) a c))
    pre-cond-not-defined (l (a c s) (sc-pre-cond (q ifndef) a c))
    pre-define-if-not-defined sc-pre-define-if-not-defined
    pre-define sc-pre-define
    pre-include-guard-begin
    (l (a c s)
      (let (id (sc-identifier (first a))) (string-append "#ifndef " id "\n" "#define " id "\n")))
    pre-include-guard-end (l (a c s) "#endif")
    pre-if-defined (l (a c s) (sc-pre-if (q ifdef) a c))
    pre-if (l (a c s) (sc-pre-if (q if) a c))
    pre-if-not-defined (l (a c s) (sc-pre-if (q ifndef) a c))
    pre-include (l (a compile state) (sc-pre-include a))
    pre-let* sc-pre-let*
    pre-let sc-pre-let*
    pre-pragma
    (l (a compile state) (string-append "#pragma " (string-join (map sc-identifier a) " ") "\n"))
    pre-concat-string (l (a c s) (string-join (map c a) " "))
    pre-stringify (l (a c s) (cp-stringify (c (first a))))
    pre-undefine
    (l (a compile state) (string-join (map (compose cp-undef sc-identifier) a) "\n" (q suffix)))
    return (l (a c s) (if (null? a) "return" (sc-apply (q return) a c s)))
    sc-comment sc-comment
    sc-define-syntax sc-define-syntax
    sc-define-syntax* sc-define-syntax*
    sc-include-once sc-include-sc-once
    sc-include sc-include-sc
    sc-insert (l (a c s) (first a))
    sc-no-semicolon sc-no-semicolon
    sc-concat (l (a c s) (apply string-append (map sc-identifier a)))
    set (sc-set-f "=")
    set* (sc-set-f "*=")
    set+ (sc-set-f "+=")
    set- (sc-set-f "-=")
    set/ (sc-set-f "/=")
    set% (sc-set-f "%=")
    struct-get (l (a c s) (apply c-struct-get (map c a)))
    struct (l (a c s) (sc-struct-or-union (q struct) a c s))
    struct-literal sc-struct-literal
    struct-pointer-get (l (a c s) (apply c-struct-pointer-get (map c a)))
    struct-set (sc-struct-set-f (q struct-get))
    struct-pointer-set (sc-struct-set-f (q struct-pointer-get))
    union (l (a c s) (sc-struct-or-union (q union) a c s)) while sc-while))

(define (sc->c* a state)
  "like sc->c but with less options and does not do a preliminary syntax check"
  (let (compile (l (a) (sc->c* a state)))
    (if (list? a)
      (let* ((f (ht-ref sc-syntax-table (first a))) (b (and f (f (tail a) compile state))))
        (if b (if (list? b) (sc->c* b state) b) (sc-apply (first a) (tail a) compile state)))
      (sc-value a))))

(define* (sc->c a #:optional state-or-load-paths enable-square-brackets)
  "expression [(string ...) sc-state] -> string
   load-paths is only used if state is not given or false"
  "disables square/round ambiguity to support type[][3] identifiers"
  (if (not enable-square-brackets) (read-disable (quote square-brackets)))
  (let
    (state
      (if (vector? state-or-load-paths) state-or-load-paths
        (sc-state-new (or state-or-load-paths (sc-default-load-paths)) #f)))
    (and (sc-syntax-check (list a) state) (sc->c* a state))))

(sc->c* sc-syntax-extensions (sc-state-new (sc-default-load-paths)))
