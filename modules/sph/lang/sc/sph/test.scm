(define-module (sph lang sc sph test))

(use-modules (ice-9 threads) (rnrs eval)
  (sph lang sc sph) (sph lang sc sph alist)
  (sph lang sc sph filesystem) (sph lang sc sph list)
  (sph lang sc sph module)
  (sph lang sc sph other) (sph lang sc sph string) (sph lang sc sph test base) (sph lang sc sph test report) (srfi srfi-1))

(export assert-and assert-equal
  assert-test-result assert-true
  define-procedure-tests define-test
  define-test-module sph-test-description
  test-execute-module test-execute-modules
  test-execute-modules-by-prefix test-execute-procedures
  test-execute-procedures-lambda test-lambda
  test-list test-module-name-from-files
  test-settings-default test-settings-default-custom
  test-settings-default-custom-by-list test-success?)

(re-export test-create-result test-result-success? test-result)

(define sph-test-description
  "automated code testing with composable modules.
   procedure-wrap: false/procedure:{test-proc test-name -> proc}
   # define-test-module
   test modules are typical modules/libraries that export only one procedure named \"execute\". the syntax defined here abstracts the definition.
   modules are used to give test modules a separated scope in which test module dependencies are integrated.
   there might be alternatives to using modules - for example evaluating files in custom environments that use the import statement. but modules seem, because of their
   possible composition, like the best solution at this point.
   the syntax defined here must be available in the environment in which the test module is loaded.
   to archieve this, the definition is initially evaluated in the top-level environment (loading/definition) and the module object is later resolved using environment* (resolving)
   # data structures:
   test-result: ([group-name] test-result ...)/vector
   # syntax
   test-settings-default-custom :: [any:unquoted-key any:value] ... -> list
   test-list :: symbol:name/(symbol:name any:io-data ...) ... -> ((symbol procedure [arguments expected] ...) ...)
   define-test
     :: (name [arguments expected settings]) body ...
     :: name procedure body ...
     define a procedure with the name test-{name} and the arity expected by the default evaluators
   define-procedure-tests :: symbol:name symbol/literal-list
     define a variable with tests that can be executed with test-execute-procedures.
     resolves procedures by name and normalises the test specification
   test-execute-procedures-lambda :: test-list-arguments ... -> procedure:{settings -> test-result}
     create a procedure that executes procedure tests corresponding to test-spec.
     can be used as the last expression in a test module
   test-lambda
     :: symbol:formals/([arguments expected settings]) body ... -> procedure
     creates a normalised test procedure with the correct arity
   assert-and :: [optional-title] expression ... -> vector:test-result
     creates a successful test result if all given expressions or assertions are successful test results or true
   assert-equal :: [optional-title] expected expression -> vector:test-result")

(define test-settings-default
  (alist-q reporters test-reporters-default
    reporter-name (q default)
    search-type (q prefix)
    hook
    (alist-q procedure-before ignore
      procedure-after ignore
      procedure-data-before ignore
      procedure-data-after ignore
      module-before ignore module-after ignore modules-before ignore modules-after ignore)
    exception->key? #f procedure-wrap #f random-order? #f parallel? #f exclude #f only #f until #f))

(define-syntax-rule (test-settings-default-custom key/value ...)
  (test-settings-default-custom-by-list (quote-odd key/value ...)))

(define (test-settings-default-custom-by-list key/value)
  "[key value] ... -> list
   get the default test settings, with values possibly set to the values given with \"key/value\""
  (alist-merge test-settings-default (list->alist key/value)))

(define (settings->hook a name) "list symbol -> procedure" (alists-ref a (q hook) name))

(define (settings->reporter a) "list -> (procedure . alist:hooks)"
  (test-reporter-get (alist-ref-q a reporters) (alist-ref-q a reporter-name)))

(define (settings->reporter-hook a name) "hashtable -> (report-write . report-hooks)"
  (alist-ref (tail (settings->reporter a)) name))

(define (identity-if-list settings maybe-new-settings)
  (if (list? maybe-new-settings) maybe-new-settings settings))

(define (call-settings-update-hook proc settings . a)
  (identity-if-list settings (apply proc settings a)))

(define (apply-settings-reporter+hook settings name . a) "list symbol any ... -> list"
  (apply (settings->reporter-hook settings name) settings a)
  (identity-if-list (apply (settings->hook settings name) settings a) settings))

(define (apply-settings-hook+reporter settings name . a) "list symbol any ... ->"
  (apply (settings->hook settings name) settings a)
  (apply (settings->reporter-hook settings name) settings a))

(define (path->load-path-and-path& a c) "string procedure:{? string -> any} -> any"
  (let (load-path (path->load-path a))
    (if load-path (c load-path a) (let (a (string-append a ".scm")) (c (path->load-path a) a)))))

(define (test-module-name-from-files a) "string -> list/error"
  (path->load-path-and-path& a
    (l (load-path a) "assumes that readlink* fails on circular symlinks"
      (if load-path
        (case (stat:type (stat (string-append load-path "/" a)))
          ((directory) (module-find-by-name (module-file->name a) (q prefix) %load-path))
          ((regular) (list (module-file->name a)))
          ((symlink) (test-module-name-from-files (readlink* a))))
        (raise (pair (q file-not-found-in-load-path) a))))))

(define-syntax-cases test-lambda s
  (((arguments expected settings) body ...) (syntax (lambda (arguments expected) body ...)))
  ( ( (a ...) body ...)
    (quasisyntax (lambda (a ... unsyntax (datum->syntax s (gensym "define-test"))) body ...))))

(define-syntax-cases define-test
  ( ( (name parameter ...) body ...)
    (syntax (define-test name (test-lambda (parameter ...) body ...))))
  ( (name proc)
    (let (name-datum (syntax->datum (syntax name)))
      (quasisyntax
        (define (unsyntax (datum->syntax (syntax name) (symbol-append (q test-) name-datum))) proc)))))

(define (macro?* a) (false-if-exception (macro? (module-ref (current-module) a))))

(define-syntax-cases test-list-one
  ( ( (name data ...))
    (let*
      ( (name-datum (syntax->datum (syntax name)))
        (test-name (datum->syntax (syntax name) (symbol-append (q test-) name-datum))))
      (quasisyntax
        (pairs (quote name)
          (eval
            (quote
              (if (defined? (quote (unsyntax test-name))) (unsyntax test-name)
                (unsyntax
                  (if (macro?* name-datum)
                    (syntax (raise (pair (q macro-instead-of-test-procedure) (quote name))))
                    (syntax (lambda (arguments . rest) (apply name arguments)))))))
            (current-module))
          (quasiquote (data ...))))))
  ((name) (syntax (test-list-one (name)))))

(define-syntax-rule (test-list test-spec ...) (list (test-list-one test-spec) ...))

(define-syntax-rule (define-procedure-tests name test-spec ...)
  " -> ((symbol procedure any ...) ...)" (define name (test-list test-spec ...)))

(define-syntax-rule (test-execute-procedures-lambda test-spec ...)
  ((l (tests) (l (settings) (test-execute-procedures settings tests))) (test-list test-spec ...)))

(define-syntax define-test-module
  (l (s)
    "using eval because exporting bindings created with define created by syntax was not working"
    (syntax-case s (import)
      ( (_ (name-part ...) (import spec ...) body ... proc)
        (syntax
          (eval
            (quote
              (library (name-part ...) (export test-execute) (import (guile) (rnrs base) (sph lang sc sph) (sph lang sc sph test) spec ...) body ... (define test-execute proc)))
            (current-module))))
      ((_ (name-part ...) body ...) (syntax (define-test-module (name-part ...) (import) body ...))))))

(define (test-module-execute settings module index name)
  "alist module/environment (symbol ...) -> test-result"
  (let*
    ( (settings (alist-set-multiple-q settings current-module-name name))
      (settings (apply-settings-reporter+hook settings (q module-before) index name))
      (r ((eval (q test-execute) module) settings)))
    (apply-settings-hook+reporter settings (q module-after) index name r) r))

(define (test-modules-until a value) (take-while (l (a) (not (equal? a value))) a))
(define (test-modules-only a values) (intersection a values))
(define (test-modules-exclude a values) (remove (l (a) (contains? values a)) a))

(define (test-modules-apply-settings settings modules)
  "list ((symbol ...) ...) -> ((symbol ...) ...)
   apply settings to a list of test-module names"
  (alist-bind settings (only exclude until)
    ( (if until test-modules-until (l (a b) a))
      (if only (test-modules-only modules only)
        (if exclude (test-modules-exclude modules exclude) modules))
      until)))

(define (test-module-success? a)
  (if (vector? a) (test-result-success? a) (if (list? a) (every test-module-success? a) a)))

(define (test-modules-execute settings module-names) "list list -> test-result"
  (let*
    ( (settings (apply-settings-reporter+hook settings (q modules-before) module-names))
      (r
        (fold-multiple-c
          (l (a continue index r)
            (let*
              ( (name (first a)) (module (tail a))
                (r-test (test-module-execute settings module index name))
                (r-group (pair (any->string name) r-test)))
              (if (test-module-success? r-test) (continue (+ 1 index) (pair r-group r))
                (list index r-group))))
          (map pair module-names (map environment* module-names)) 0 (list)))
      (r (if (list? r) (reverse (second r)) r)))
    (apply-settings-hook+reporter settings (q modules-after) module-names r) r))

(define (settings->load-path! a)
  "list -> (string ...)
   adds the value for the \"path-search\" setting to the global load-path and returns the value in a list to be used as a load-path variable, if the path-search value is not false.
   otherwise returns %load-path"
  (let (path-search (alist-ref-q a path-search))
    (if path-search (let (path (path->full-path path-search)) (add-to-load-path path) (list path))
      %load-path)))

(define (test-modules-by-prefix-execute settings . name)
  "list (symbol ...) ... -> test-result
   execute all test-modules whose names begin with name-prefix.
   for example if there are modules (a b c) and (a d e), (test-execute-modules (a)) will execute both
   modules must be in load-path. modules for testing are libraries/guile-modules that export an \"execute\" procedure.
   this procedure is supposed to return a test-result, for example from calling \"test-execute\""
  (let
    ((load-path (settings->load-path! settings)) (search-type (alist-ref-q settings search-type)))
    (let
      (module-names
        (every-map (l (a) (false-if-null (module-find-by-name a search-type load-path))) name))
      (if module-names
        (test-modules-execute settings
          (test-modules-apply-settings settings (apply append module-names)))
        (raise (pair (q module-not-found) name))))))

(define (filter-module-names a) "list -> list" (filter list? a))
(define (filter-procedure-names a) "list -> list" (filter symbol? a))

(define (test-procedures-until a value) "list symbol -> list"
  (take-while (l (spec) (not (equal? (first spec) value))) a))

(define (test-procedures-only a values) "list list -> list"
  (let (values (filter-procedure-names values))
    (if (null? values) a (filter (l (spec) (containsv? values (first spec))) a))))

(define (test-procedures-exclude a values) "list list -> list"
  (let (values (filter-procedure-names values))
    (remove (l (spec) (containsv? values (first spec))) a)))

(define (test-procedures-apply-settings settings a) "list list -> list"
  (alist-bind settings (only exclude until random-order?)
    ( (if random-order? randomize identity)
      ( (if until test-procedures-until (l (a b) a))
        (if only (test-procedures-only a only) (if exclude (test-procedures-exclude a exclude) a))
        until))))

(define (test-any->result result title index)
  (test-create-result (eqv? #t result) title #f index result (list) #t))

(define
  (test-procedures-execute-one-data settings index-procedure data name title test-proc hook-before
    hook-after
    hook-data-before
    hook-data-after
    report-before
    report-after
    report-data-before
    report-data-after)
  "list (input output [input output] ...) string string procedure ... -> test-result"
  (let loop ((d data) (index 0))
    (if (null? d) (test-create-result #t title #f index)
      (let*
        ( (arguments (any->list (first d)))
          (settings
            (begin (report-data-before settings index-procedure name index arguments)
              (call-settings-update-hook hook-data-before settings
                index-procedure name index arguments)))
          (d-tail (tail d)) (expected (first d-tail))
          (r (test-proc arguments expected settings))
          (r
            (if (test-result? r)
              (begin (test-result-title-set! r title) (test-result-index-set! r index) r)
              (test-create-result (equal? r expected) title #f index r arguments expected))))
        (hook-data-after settings index-procedure index r)
        (report-data-after settings index-procedure index r)
        (if (test-result-success? r) (loop (tail d-tail) (+ 1 index)) r)))))

(define
  (test-procedures-execute-without-data settings index name title test-proc hook-before hook-after
    hook-data-before
    hook-data-after
    report-before
    report-after
    report-data-before
    report-data-after)
  (report-data-before settings index name 0 (list))
  (let*
    ( (settings (call-settings-update-hook hook-data-before settings index name 0 (list)))
      (r (test-proc (list) #t settings))
      (r
        (if (test-result? r)
          (begin
            (test-result-title-set! r
              (let (sub-title (test-result-title r))
                (if sub-title (string-append title " " sub-title) title)))
            r)
          (test-any->result r title 0))))
    (hook-data-after settings index 0 r) (report-data-after settings index 0 r) r))

(define (test-procedure-wrap settings test-proc test-name)
  (let (wrap (alist-ref settings (q procedure-wrap)))
    (if (procedure? wrap) (wrap test-proc test-name) test-proc)))

(define (test-exception->key settings test-proc) test-proc)

(define (test-procedures-execute-one settings index hooks name test-proc . data)
  "procedure:{test-result -> test-result} procedure [arguments expected] ... -> vector:test-result"
  (list-bind hooks
    (hook-before hook-after hook-data-before
      hook-data-after report-before report-after report-data-before report-data-after)
    (let
      ( (title (symbol->string name))
        (test-proc (test-exception->key settings (test-procedure-wrap settings test-proc name))))
      (report-before settings index name)
      (let*
        ( (settings (call-settings-update-hook hook-before settings index name))
          (r
            (if (null? data)
              (apply test-procedures-execute-without-data settings index name title test-proc hooks)
              (apply test-procedures-execute-one-data settings
                index data name title test-proc hooks))))
        (hook-after settings index r) (report-after settings index r) r))))

(define (test-procedures-execute-parallel settings a hooks)
  "list:alist list -> list
   executes all tests even if some fail"
  (par-map (l (e) (apply test-procedures-execute-one settings 0 hooks e)) a))

(define (test-procedures-execute-serial settings a hooks)
  "list:alist list -> list
   executes tests one after another and stops if one fails"
  (let
    (r
      (fold-multiple-c
        (l (e continue index r)
          (let (r-test (apply test-procedures-execute-one settings index hooks e))
            (if (test-result-success? r-test) (continue (+ 1 index) (pair r-test r))
              (list index (list r-test)))))
        a 0 (list)))
    (if (list? r) (reverse (second r)) r)))

(define test-procedures-execute
  (let
    ( (get-executor
        (l (settings) "list -> procedure"
          (if (alist-ref-q settings parallel?) test-procedures-execute-parallel
            test-procedures-execute-serial)))
      (settings->procedure-hooks
        (l (settings) "list -> (procedure:hook-before hook-after report-before report-after)"
          (append
            (alist-select (alist-ref-q settings hook)
              (q (procedure-before procedure-after procedure-data-before procedure-data-after)))
            (alist-select (tail (settings->reporter settings))
              (q (procedure-before procedure-after procedure-data-before procedure-data-after)))))))
    (l (settings source)
      "list ((symbol:name procedure:test-proc any:data-in/out ...) ...) -> test-result"
      ( (get-executor settings) settings (test-procedures-apply-settings settings source)
        (settings->procedure-hooks settings)))))

(define (test-execute-module settings name) "list (symbol ...) -> test-result"
  (test-module-execute settings (environment* name) 0 name))

(define test-execute-procedures test-procedures-execute)
(define test-execute-modules test-modules-execute)

(define*
  (test-execute-modules-by-prefix #:key (settings test-settings-default) #:rest module-names)
  "execute all test modules whose module name has the one of the given module name prefixes.
   \"path-search\" restricts the path where to search for test-modules.
   \"search-type\" does not execute modules that exactly match the module name prefix (where the module name prefix resolves to a regular file)"
  (apply test-modules-by-prefix-execute settings (remove-keyword-associations module-names)))

(define (assert-failure-result result expected title arguments)
  "vector/any any false/string any -> vector:test-result"
  (if (test-result? result)
    (if (string? title) (begin (test-result-assert-title-set! result title) result) result)
    (if (string? title) (test-create-result #f title #f #f result arguments expected)
      (test-create-result #f #f "assertion" #f result arguments expected))))

(define-syntax-rules assert-true
  ( (optional-title expr)
    (let (r expr) (if r #t (assert-failure-result r #t optional-title (q expr)))))
  ((expr) (assert-true #f expr)))

(define-syntax-rule (assert-test-result title expr continue)
  "string/false expression expression -> true/test-result:failure
   assertions create test-result vectors only on failure"
  (let (r expr)
    (if (and r (or (boolean? r) (and (test-result? r) (test-result-success? r)))) continue
      (assert-failure-result r #t title (q expr)))))

(define-syntax-rules assert-and-with-explicit-title ((title a) (assert-test-result title a #t))
  ( (title a a-rest ...)
    (assert-test-result title a (assert-and-with-explicit-title title a-rest ...))))

(define-syntax-case (assert-and optional-title expr ...)
  (let (optional-title-datum (syntax->datum (syntax optional-title)))
    (if (string? optional-title-datum)
      (syntax (assert-and-with-explicit-title optional-title expr ...))
      (syntax (assert-and-with-explicit-title #f optional-title expr ...)))))

(define-syntax-rules assert-equal
  ( (optional-title expected expr)
    (let ((r expr) (exp expected))
      (if (equal? exp r) #t (assert-failure-result r exp optional-title (q expr)))))
  ((expected expr) (assert-equal #f expected expr)))
