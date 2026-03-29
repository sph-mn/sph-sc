(define-module (sph lang sc sph install))

(use-modules ((srfi srfi-1) #:select (find second every)) (ice-9 match)
  (sph lang sc sph) ((sph lang sc sph alist) #:select (alist-q alist-ref))
  (sph lang sc sph cli) ((sph lang sc sph string) #:select (string-quote any->string))
  ((sph lang sc sph other) #:select (remove-keyword-associations))
  ( (sph lang sc sph filesystem) #:select
    (copy-file-recursive directory? ensure-trailing-slash ensure-directory-structure-and-new-mode)))

(export install install-cli sph-install-description install-cli-p)

(define sph-install-description
  "copy files and set permissions, with an optional, automatically created command line interface for users to set install options.
   example
     (install-cli
       (\"/tmp\" \"source/myfile\" \"source/mydir\")
       (system-executables 700 \"exe/sc\")
       (guile-site-modules (600 700) \"modules/sph\" \"modules/test\"))
   syntax
     install-cli :: install-config ...
       install-config will be wrapped in quasiquote, so that unquote and unquote-splicing can be used.
       arguments will be passed to install-cli-p.
       default options can be set, corresponding to the keyword arguments that \"install\" accepts.
       (install-cli
         (options #:directory-mode 700)
         (system-executables 700 \"exe/sc\"))")

(define (dry-run-log action . a)
  (display (string-append action " " (string-join (map any->string a) " "))) (newline) #t)

(define symlink-file symlink)

(define (placeholder-guile-site-modules)
  (let (site (find (l (a) (string-suffix? "/site" a)) %load-path))
    (or site "/usr/share/guile/site")))

(define (select-mode-by-file-type a regular-mode directory-mode)
  (if (eq? (q directory) (stat:type (stat a))) directory-mode regular-mode))

(define (parse-octal-integer a) (string->number (if (number? a) (number->string a) a) 8))

(define install-cli-parser
  (cli-create #:options
    (qq
      ( (target-prefix #:value-required? #t
          #:type string #:description "prepended to each destination path")
        (symlink #:description "create symlinks instead of file copies")
        (dry-run #:description "make no changes and only list the actions that would be executed")
        (directory-mode #:value-required? #t
          #:type integer #:description "default permissions for directories in octal notation")
        (regular-mode #:value-required? #t
          #:type integer #:description "default permissions for regular files in octal notation")
        (placeholders #:value-required? #t
          #:type string
          #:description
          "override default placeholder values. example: --placeholders=\"system-executables=/usr/bin;guile-site-modules=/usr/share/guile/site\"")
        (placeholders-list #:description "display the current values for used placeholders")))))

(define (parse-placeholders-string a)
  (if a
    (map (l (a) (let (a (string-split a #\=)) (pair (string->symbol (first a)) (second a))))
      (string-split a #\;))
    null))

(define (get-placeholders overrides) "-> ((key . value) ...)"
  (alist-q guile-site-modules
    (or (alist-ref overrides (q guile-site-modules)) (placeholder-guile-site-modules))
    system-executables (or (alist-ref overrides (q system-executables)) "/usr/bin")
    system-libraries (or (alist-ref overrides (q system-libraries)) "/usr/lib")))

(define (normalise-install-configs a regular-mode directory-mode)
  (map
    (l (a)
      (match a
        ( (target ((? integer? regular) (? integer? directory)) sources ...)
          (pairs target (pair (parse-octal-integer regular) (parse-octal-integer directory))
            sources))
        ( (target (? integer? mode) sources ...)
          (pairs target (pair (parse-octal-integer mode) directory-mode) sources))
        ((target sources ...) (pairs target (pair regular-mode directory-mode) sources))))
    a))

(define (handle-existing-target source target symlink)
  "true if target should not be changed.
   remove existing target if symlink to a different file"
  (if (eq? (q symlink) (stat:type (lstat target)))
    (if (and symlink (= (stat:ino (stat source)) (stat:ino (stat target)))) #t
      (begin (delete-file target) #f))
    (if symlink (begin (delete-file target) #f) #f)))

(define (copy-file-proc mode symlink dry-run)
  (if dry-run
    (l (source target)
      (let ((source (string-quote source)) (target (string-quote target)))
        (dry-run-log (if symlink "ln -s" "cp") source target))
      (dry-run-log "chmod" (number->string (if (directory? source) (tail mode) (first mode)) 8)
        target))
    (l (source target)
      (if (not (and (file-exists? target) (handle-existing-target source target symlink)))
        (begin ((if symlink symlink-file copy-file) source target)
          (chmod target (if (directory? source) (tail mode) (first mode))))))))

(define (ensure-target-directory target directory-mode dry-run)
  (if dry-run
    (if (not (file-exists? target))
      (begin (dry-run-log "mkdir -p" target)
        (dry-run-log "chmod" (number->string directory-mode 8) target)))
    (ensure-directory-structure-and-new-mode target directory-mode)))

(define (every-install-config install-configs target-prefix placeholders directory-mode dry-run f)
  "call f with (source target mode) of each install config and stop if f returns false"
  (every
    (l (install-config)
      (apply
        (l (target mode . sources)
          (let
            (target
              (string-append target-prefix
                (if (symbol? target) (alist-ref placeholders target) target)))
            (ensure-target-directory target directory-mode dry-run)
            (every (l (source) (f source target mode)) sources)))
        install-config))
    install-configs))

(define*
  (install install-configs #:key target-prefix regular-mode directory-mode symlink dry-run
    placeholders)
  "install into target directories and set filesystem permissions.
   install-configs is of the format ((string/symbol:target-path [mode] source-path ...) ...)
   mode is optional and a decimal integer that will be parsed as if octal, for example 777 will become octal 777.
   * copies source directories recursively
   * option to symlink instead of copying
   * placeholder for guile site directory
   * default permissions are 644 and 755 for files and directories respectively"
  (let*
    ( (target-prefix (or target-prefix ""))
      (regular-mode (parse-octal-integer (or regular-mode 644)))
      (directory-mode (parse-octal-integer (or directory-mode 755)))
      (placeholders (get-placeholders placeholders))
      (install-configs
        (normalise-install-configs (remove-keyword-associations install-configs) regular-mode
          directory-mode)))
    (every-install-config install-configs target-prefix
      placeholders directory-mode
      dry-run
      (l (source target mode)
        (copy-file-recursive (canonicalize-path source)
          (string-append target "/" (basename source)) #:copy-file
          (copy-file-proc mode symlink dry-run) #:stop-on-error
          #t #:ensure-directory
          (l (path)
            (if dry-run
              (let (path (string-quote path)) (dry-run-log "mkdir" path)
                (dry-run-log "chmod" (number->string directory-mode 8) path))
              (begin (if (not (file-exists? path)) (mkdir path)) (chmod path directory-mode)))))))))

(define (extract-config-options install-configs c)
  "the first install-config can be a list with keyword arguments for install"
  (match install-configs
    ( ( ( (quote options) config-option ...) install-configs ...)
      (apply c install-configs config-option))
    (_ (c install-configs))))

(define (display-used-placeholders install-configs placeholders)
  (let
    ( (final-placeholders (get-placeholders placeholders))
      (used-placeholders (delete (q options) (filter symbol? (map first install-configs)))))
    (each
      (l (a) (display (string-append (symbol->string a) " " (alist-ref final-placeholders a) "\n")))
      used-placeholders)))

(define* (install-cli-p install-configs)
  "like \"install\" but automatically creates a command-line interface for users to set custom options.
   the first install-config can be of the format (options install-arguments ...), where options is a symbol.
   install-arguments are defaults for the keyword arguments that \"install\" accepts and will be overridden by given command-line options"
  (let (options (install-cli-parser (tail (program-arguments))))
    (extract-config-options install-configs
      (l*
        (install-configs #:key target-prefix
          regular-mode directory-mode symlink dry-run placeholders)
        (let
          (placeholders
            (parse-placeholders-string (or (alist-ref options (q placeholders)) placeholders)))
          (if (alist-ref options (q placeholders-list))
            (display-used-placeholders install-configs placeholders)
            (install install-configs #:target-prefix
              (or (alist-ref options (q target-prefix)) target-prefix) #:regular-mode
              (or (alist-ref options (q regular-mode)) regular-mode) #:directory-mode
              (or (alist-ref options (q directory-mode)) directory-mode) #:symlink
              (or (alist-ref options (q symlink)) symlink) #:dry-run
              (or (alist-ref options (q dry-run)) dry-run) #:placeholders placeholders)))))))

(define-syntax-rule (install-cli install-config ...) (install-cli-p (qq (install-config ...))))
