(define-module (sph lang sc sph cli))

(use-modules (rnrs sorting)
  ((srfi srfi-1) #:select (drop-right any fold drop drop-while remove append-map partition))
  (srfi srfi-37) (sph lang sc sph)
  (sph lang sc sph alist)
  ( (sph lang sc sph list) #:select
    (contains? any->list list-prefix?
      map-apply pattern-match-min-length first-or-null containsv? fold-multiple split-by-pattern))
  ((sph lang sc sph string) #:select (string-multiply any->string-write)))

(export cli-create cli-command-match cli-option-spec->list sph-cli-description)
(define sph-cli-description "create command-line interfaces")
(define help-text-line-description-delimiter "  ")
(define indent "  ")
(define options-parameter "options ...")

(define (typecheck+conversion type a)
  (cond
    ((eqv? (q string) type) a)
    ((eqv? (q number) type) (string->number a))
    ((eqv? (q integer) type) (let (a (string->number a)) (and (integer? a) a)))
    ((list? type) (any (l (type) (typecheck+conversion type a)) type))
    (else a)))

(define (processor opt name a r) (pair a r))
(define (unnamed-processor a r) (pair a r))
(define (unrecognized-processor opt name a r) (throw (q unsupported-option) opt name))

(define (display-version-proc version-spec)
  (l (opt name a r)
    (display
      (string-append
        (if (list? version-spec) (string-join (map number->string version-spec) ".")
          (number->string version-spec))
        "\n"))
    (exit 0)))

(define (unnamed-option? a) "list -> boolean"
  (or (list? (first a)) (and (not (null? (tail a))) (null? (first (tail a))))))

(define (format-argument-name input-type required? name-prepend) "false/symbol boolean -> string"
  (let (name (if input-type (symbol->string input-type) "value"))
    (if required? (string-append name-prepend name)
      (if (string-null? (string-trim name-prepend)) (string-append name-prepend "[" name "]")
        (string-append "[" name-prepend name "]")))))

(define
  (named-option->help-text-line name/pattern names required? value-required? value-optional?
    input-type
    description
    custom-processor)
  (let
    (format-argument-name*
      (l (string-prepend)
        (if value-required? (format-argument-name input-type value-required? string-prepend)
          (if value-optional? (format-argument-name input-type #f string-prepend) ""))))
    (string-append
      (string-join
        (map
          (l (e)
            (if (string? e) (string-append "--" e (format-argument-name* "="))
              (string-append (string #\- e) (format-argument-name* " "))))
          (if names (pair (symbol->string name/pattern) (any->list names))
            (list (symbol->string name/pattern))))
        " | ")
      (if description (string-append help-text-line-description-delimiter description) ""))))

(define
  (unnamed-option->help-text-line name/pattern names value-required? value-optional? input-type
    description
    custom-processor)
  (if (list? name/pattern)
    (let (name/pattern (delete (q ...) name/pattern))
      (if (list? description)
        (map
          (l (name description)
            (string-append (symbol->string name) help-text-line-description-delimiter description))
          name/pattern
          (let (length-difference (- (length name/pattern) (length description)))
            (if (> length-difference 0) (append description (make-list "")) description)))
        (string-append
          (if description
            (string-append (symbol->string (first name/pattern))
              help-text-line-description-delimiter description "\n")
            "")
          (string-join (map symbol->string (tail name/pattern)) "\n"))))
    (string-append (symbol->string name/pattern)
      (if description (string-append help-text-line-description-delimiter description) ""))))

(define (string-join-lines-with-indent a indent) "(string ...) string -> string"
  (string-join a (string-append "\n" indent) (q prefix)))

(define (options->help-text-lines a) "list -> string"
  (map
    (l (e)
      (apply (if (unnamed-option? e) unnamed-option->help-text-line named-option->help-text-line)
        (apply option-spec->list e)))
    (list-sort (l (a b) (string< (symbol->string (first a)) (symbol->string (first b)))) a)))

(define (options-spec->unnamed-arguments-strings a)
  (map
    (l (a)
      (let*
        ( (explicitly-not-required
            (and (contains? (tail a) #:required?)
              (not (apply (l* (#:key required? #:allow-other-keys) required?) (tail a)))))
          (a (string-join (map symbol->string (first a)) " ")))
        (if explicitly-not-required (string-append "[" a "]") a)))
    (filter unnamed-option? a)))

(define (commands->help-text-lines a) "list:commands-spec -> string"
  (list-sort string<?
    (map
      (l (a)
        (let*
          ( (command (string-join (first a) " ")) (command-arguments (tail a))
            (options
              (if (null? command-arguments) null
                (if (null? (tail command-arguments))
                  (if (procedure? (first command-arguments)) null
                    (keyword-list->alist+keyless command-arguments))
                  (keyword-list->alist+keyless command-arguments)))))
          (string-append command
            (if (null? options) ""
              (apply
                (l (named . unnamed)
                  (let
                    ( (unnamed (options-spec->unnamed-arguments-strings unnamed))
                      (description (alist-ref-q named description)))
                    (string-append (if (null? unnamed) "" (string-append " :: " (first unnamed)))
                      (if description (string-append " | " description) ""))))
                options)))))
      a)))

(define (options-remove-processors a)
  "(single-option-spec ...) -> list
   remove processor procedures from option-specs"
  (map (l (e) (reverse (drop-while not (reverse (if (> (length e) 6) (drop-right e 1) e))))) a))

(define (display-command-line-interface-proc config spec)
  (l arguments (write (options-remove-processors spec)) (exit 0)))

(define (config->usage-text a spec)
  (let
    (arguments
      (map (l (e) (string-append options-parameter " " e))
        (options-spec->unnamed-arguments-strings spec)))
    (string-append "usage\n" indent
      (if (null? arguments) options-parameter (string-join arguments (string-append "\n" indent))))))

(define (format-help-description a indent)
  "string string -> string
   multiline strings in scheme are often indented in code, but the whitespace is not intended to be part of the string.
   this procedure removes existing indent and applies the given one"
  (string-join (map (l (a) (string-trim a #\space)) (string-split a #\newline))
    (string-append "\n" indent) (q prefix)))

(define (display-help-proc text commands command-options config options)
  (l (opt name a r)
    (display
      (string-append
        (let (a (alist-ref config (q help-usage))) (or a (config->usage-text config options)))
        (if (and text (not (string-null? text)))
          (string-append "\ndescription" (format-help-description text indent)) "")
        "\noptions"
        (string-join-lines-with-indent (options->help-text-lines (remove unnamed-option? options))
          indent)
        (if (null? command-options) ""
          (string-append "\noptions shared by all commands"
            (string-join-lines-with-indent
              (options->help-text-lines (remove unnamed-option? command-options)) indent)))
        (if commands
          (string-append "\ncommands"
            (string-join-lines-with-indent (commands->help-text-lines commands) indent))
          "")
        "\n"))
    (exit 0)))

(define (display-about-proc text config)
  (l (opt name a r) (display (string-append (if (procedure? text) (text) text) "\n")) (exit 0)))

(define (add-typecheck type c)
  (if type
    (l (opt name value r)
      (if value
        (let (value-after (typecheck+conversion type value))
          (if value-after (c opt name value-after r)
            (begin
              (simple-format #t
                "error wrong-type-for-argument option-name:~A expected-type:~A given-argument:~S\n"
                name type value)
              (exit 1))))
        (c opt name value r)))
    c))

(define (option-spec->list . arguments) (apply option-spec-variables& list arguments))

(define*
  (option-spec-variables& c name #:key names required? value-required? value-optional? type
    description
    processor)
  (c name names required? value-required? value-optional? type description processor))

(define* (option-spec->args-fold-option a)
  (apply
    (l
      (name names required? value-required? value-optional? input-type description custom-processor)
      (option
        (let
          ( (names (if names (if (list? names) names (list names)) names))
            (name-string (symbol->string name)))
          (if names (if (contains? names name-string) names (append names (list name-string)))
            (list (symbol->string name))))
        value-required? value-optional?
        (add-typecheck input-type
          (l (opt matched-name a r)
            (if custom-processor (custom-processor opt matched-name a r)
              (pair (pair name (if a a #t)) r))))))
    a))

(define (config->options-default-options a commands command-options-config)
  "alist:config -> (procedure:{options -> list:extended-options/false})"
  (list
    (l (options)
      (let (version-spec (alist-ref-q a version))
        (and version-spec
          (pair (q (version #:names #\v #:processor (display-version-proc version-spec))) options))))
    (l (options)
      (let (text (alist-ref-q a about))
        (and text
          (pair (qq (about #:names #\a #:processor (unquote (display-about-proc text a)))) options))))
    (l (options)
      (let
        ( (help-option (q (help #:names #\h #:description "show this help text")))
          (cli-option (q (interface #:description "show a machine readable cli specification"))))
        (let*
          ( (options-temp (pairs cli-option help-option options))
            (options
              (pair
                (append help-option
                  (list #:processor
                    (display-help-proc (alist-ref-q a description) commands
                      command-options-config a options-temp)))
                options)))
          (pair
            (append cli-option
              (list #:processor (display-command-line-interface-proc a options-temp)))
            options))))))

(define (config->options a options-config commands command-options-config) "config list -> list"
  (map-apply option-spec->list
    (remove unnamed-option?
      (fold (l (default-option-proc options) (or (default-option-proc options) options))
        (if options-config options-config (list))
        (config->options-default-options a commands command-options-config)))))

(define (match-unnamed-options spec unnamed parsed)
  "list list list:matches -> (list:rest list:matches)"
  (let (pattern (first spec))
    (apply
      (l (match rest)
        (if match (list rest (pair match parsed))
          (if (and (> (length spec) 2) (list-ref spec 2))
            (let (unnamed-length (length unnamed))
              (throw (q missing-arguments) (- (pattern-match-min-length pattern) unnamed-length)
                (drop pattern unnamed-length)))
            (list unnamed parsed))))
      (split-by-pattern pattern unnamed))))

(define (process-unnamed-options parsed-options unnamed-option-specs) "list list ->"
  (call-with-values (l () (partition string? parsed-options))
    (l (unnamed named)
      (apply
        (l (unnamed parsed)
          (append (if (null? unnamed) unnamed (list (pair (q unnamed) unnamed)))
            (apply append parsed) named))
        (fold-multiple match-unnamed-options unnamed-option-specs unnamed (list))))))

(define (check-required a option-specs) "list:options list:option-specs ->"
  (let*
    ( (given (alist-keys a))
      (missing
        (map first
          (filter
            (l (e) (and (if (> (length e) 2) (list-ref e 2) #f) (not (containsv? given (first e)))))
            option-specs))))
    (if (null? missing) a (throw (q missing-arguments) (length missing) missing))))

(define (default-missing-arguments-handler key count option-names)
  "symbol integer integer (string ...) ->
   write message to standard output and exit"
  (format #t "~a missing argument~p ~s\n" count count option-names) (exit 1))

(define (default-unsupported-option-handler key option option-name)
  "symbol srfi-37-option string ->
   write message to standard output and exit"
  (format #t "unsupported option ~s\n" option-name) (exit 1))

(define (cli-command-match arguments commands-spec) "list list -> false/any"
  (any (l (e) (if (list-prefix? arguments (first e)) e #f)) commands-spec))

(define (command-dispatch& command-handler arguments commands-spec command-options c)
  "procedure/false list list/false procedure:{-> any} -> any:end-result-of-cli-application
   if a command from commands-spec is matched at the beginning of the given cli arguments, eventually calls an associated handler procedure,
   and in any case calls the command-handler if available.
   if no command matches, proceeds with nullary \"c\""
  (if commands-spec
    (let (match (cli-command-match arguments commands-spec))
      (if match
        (apply
          (l (command . command-arguments)
            (let (rest-arguments (list-tail arguments (length command)))
              ( (or command-handler (l (a b) b)) command
                (if (procedure? command-arguments) (command-arguments command rest-arguments)
                  ( (apply cli-create
                      (if command-options (append command-options command-arguments)
                        command-arguments))
                    rest-arguments)))))
          match)
        (c)))
    (c)))

(define (config->commands a) "list -> list"
  (false-if-exception
    (map
      (l (a)
        (if (string? a) (list (list a)) (if (string? (first a)) (pair (list (first a)) (tail a)) a)))
      (alist-ref a (q commands)))))

(define (config->missing-arguments-handler a) "list -> procedure/false"
  (let (v (alist-ref-q a missing-arguments-handler (q undefined)))
    (if (eqv? (q undefined) v) default-missing-arguments-handler (and (procedure? v) v))))

(define (config->unsupported-option-handler a) "list -> procedure/false"
  (let (v (alist-ref-q a unsupported-option-handler (q undefined)))
    (if (eqv? (q undefined) v) default-unsupported-option-handler (and (procedure? v) v))))

(define (cli-create . config)
  "::
   #:version string/(integer ...)
   #:about string/procedure:{-> string}
   #:description string
   #:usage string
   #:usage-arguments string
   #:arguments (string ...)
   #:null-arguments (string ....)
   #:missing-arguments-handler procedure:{symbol any ...}
   #:unsupported-option-handler procedure:{symbol srfi-37-option option-name ...}
   #:command-handler procedure:{list:(symbol ...):command-name list:rest-arguments -> any}
   #:commands commands-spec
   #:command-options option-spec
   #:options (option-spec ...)
   option-spec ...
   ->
   procedure:{(string ...) -> list:alist:((symbol . any) ...):parsed-arguments}

   # description
   null-arguments: arguments to use when no arguments have been given to the cli, for example (list \"--help\")
   commands-options: options shared between commands
   commands: specify sub-commands that are recognised as leading keywords in the program arguments. with options or handler procedures
   command-handler: command-handler called for all commands (after individual command handlers)
   options: specify long, short and unnamed options
   # data-structures
   custom-processor: custom handler for option values. procedure:args-fold-processor:{opt matched-name any result ->}
   input-type-names: symbol:string/number/integer
   option-spec: (symbol/list:name/pattern #:names character/string/(character/string ...) #:required? boolean
                 #:value-required? boolean #:value-optional? boolean #:type symbol #:description string #:processor procedure:custom-processor)
   pattern: (symbol symbol/ellipsis:... ...)
   commands-spec: (((string:command-name ...) procedure:{command arguments}/[cli-create-argument ...]) ...).
   more documentation and examples can be found on http://sph.mn/computer/software/sph-lib/cli.html"
  (apply cli-create-original config))

(define (cli-create-original . config)
  "without wrapping, cli-create was bound to command-dispatch& inside command-dispatch&"
  (let*
    ( (config+keyless (keyword-list->alist+keyless config)) (config (first config+keyless))
      (options-config
        (let (a (alist-ref config (q options)))
          (if a (append a (tail config+keyless)) (tail config+keyless))))
      (command-options (alist-ref config (q command-options) (list)))
      (commands (config->commands config))
      (option-specs (config->options config options-config commands command-options))
      (missing-arguments-handler (config->missing-arguments-handler config))
      (unsupported-option-handler (config->unsupported-option-handler config))
      (unnamed-options (if options-config (filter unnamed-option? options-config) (list)))
      (command-handler (alist-ref config (q command-handler)))
      (af-options (map option-spec->args-fold-option option-specs))
      (null-arguments (alist-ref config (q null-arguments))))
    (l arguments
      (let*
        ( (arguments (if (null? arguments) (tail (program-arguments)) (first arguments)))
          (no-command-cli
            (let*
              ( (arguments (or (and (null? arguments) null-arguments) arguments))
                (cli
                  (nullary
                    (check-required
                      (process-unnamed-options
                        (reverse
                          (args-fold arguments af-options
                            unrecognized-processor unnamed-processor (list)))
                        unnamed-options)
                      option-specs)))
                (cli
                  (if missing-arguments-handler
                    (nullary (catch (q missing-arguments) cli missing-arguments-handler)) cli))
                (cli
                  (if unsupported-option-handler
                    (nullary (catch (q unsupported-option) cli unsupported-option-handler)) cli)))
              cli)))
        (command-dispatch& command-handler arguments commands command-options no-command-cli)))))
