(define-module (sph lang sc sph filesystem))

(use-modules (srfi srfi-1) (srfi srfi-2)
  (ice-9 ftw) (ice-9 regex)
  (ice-9 threads) (rnrs bytevectors)
  (rnrs io ports) (sph lang sc sph) (sph lang sc sph hashtable) (sph lang sc sph list) (sph lang sc sph other) (sph lang sc sph string))

(export sph-filesystem-description call-with-directory
  copy-file-recursive directory-fold
  directory-list directory-list-full
  directory-prefix-tree directory-reference?
  directory-tree directory-tree-each
  directory-tree-leaf-directories directory?
  dotfile? system-temp-dir
  ensure-directory-structure ensure-directory-structure-and-new-mode
  ensure-trailing-slash filename-extension
  filesystem-glob fold-directory-tree
  list->path get-unique-path
  mtime-difference path->full-path
  path->list path-append
  path-append* path-directories
  poll-watch readlink*
  realpath* remove-filename-extension
  remove-trailing-slash search-load-path
  stat-accessor->stat-field-name stat-diff stat-diff->accessors stat-field-name->stat-accessor)

(define sph-filesystem-description
  "filesystem helpers.
   # highlights
   filesystem-glob: resolves ``*/**/?`` in paths
   copy-file-recursive
   ensure-directory-structure
   ensure-trailing-slash
   poll-watch: watch for filesystem stat changes
   realpath*: realpath implementation not requiring posix realpath")

(define (system-temp-dir)
  "returns the value of the %TEMP% environment variable on windows, /tmp otherwise.
   (port-filename (tmpfile)) returns #f, tmpnam is deprecated,
   mkstmp! does not choose the path - currently no alternative found"
  (if (eq? (q windows) (system-file-name-convention)) (or (getenv "TEMP") ".") "/tmp"))

(define filesystem-glob
  (let*
    ( (double-asterisk (make-regexp "^\\*\\*([0-9]+)?$"))
      (parse-skip
        (l (a) "string -> number"
          (and-let* ((a (regexp-exec double-asterisk a)))
            (let (depth (match:substring a 1)) (if depth (string->number depth) (inf))))))
      (parse-match
        (l (a) "string -> regexp/string"
          (if (string-index a (l (a) (or (eqv? a #\*) (eqv? a #\?))))
            (make-regexp
              (string-append "^"
                (string-replace-strings
                  (regexp-quote (string-replace-strings a (q (("*" . "/1/") ("?" . "/2/")))))
                  (q (("/1/" . ".*") ("/2/ " . "."))))
                "$"))
            a)))
      (scandir*
        (l (a) "string -> (string:file-name ...)/false"
          (scandir (if (string-null? a) "." a) (l (a) (not (string-prefix? "." a))))))
      (get-directory-paths
        (l (path) "string -> (string:full-path ...)"
          (filter-map (l (a) (let (a (string-append path "/" a)) (and (directory? a) a)))
            (scandir* path))))
      (parse-path
        (l (path) "string -> (number/string/regexp ...)"
          (let*
            ( (path (string-split path #\/))
              (path (if (regexp-exec double-asterisk (last path)) (append path (list "*")) path))
              (parsed (map (l (a) (or (parse-skip a) (parse-match a))) path)))
            (map-consecutive string? (l a (string-join a "/")) parsed)))))
    (l (path)
      "string -> (string ...)
       find files matching a file system path with optional wildcard characters.
       * matches zero or more of any character in a file name.
       ? matches one of any character in a file name.
       ** skips any sub directories to match the rest of the path. at the end of a path it is the same as **/.* including .*
       **n where n is an integer. like ** but skips directories at most n subdirectories deep.
       example patterns
         a/b/*
         *.txt
         a/**/c/*.txt
         a/**
         **/*.txt
             a/**2/*"
      "split path into literal and wildcard portions. check literal parts with file-exists?"
      "check wildcard parts by reading and matching directory entries."
      "to consider: result order, full/relative path"
      (reverse
        (let loop ((parsed (parse-path path)) (path "") (result null) (skip 0))
          (if (null? parsed) result
            (let (pattern (first parsed))
              (cond
                ((number? pattern) (loop (tail parsed) path result (+ pattern skip)))
                ( (string? pattern)
                  (let (pattern-path (string-append path pattern))
                    (if (file-exists? pattern-path)
                      (if (null? (tail parsed)) (pair pattern-path result)
                        (loop (tail parsed) (string-append pattern-path "/") result skip))
                      (if (< 0 skip)
                        (fold
                          (l (path result)
                            "try to match the same pattern in any next lower directory"
                            (loop parsed (string-append path "/") result (- skip 1)))
                          result (get-directory-paths path))
                        result))))
                ( (regexp? pattern) "read entries of directory, match files and collect directories"
                  (let loop2
                    ( (files (scandir* path)) (directories null) (skip-directories null)
                      (path path) (result result) (skip skip))
                    "directory has been fully read. if skip, try to match the same pattern in sub directories, then proceed with next pattern"
                    (if (null? files)
                      (fold
                        (l (path result) (loop (tail parsed) (string-append path "/") result skip))
                        (if (< 0 skip)
                          (fold
                            (l (path result) "directory paths are full paths"
                              (loop2 (scandir* path) null
                                null (string-append path "/") result (- skip 1)))
                            result skip-directories)
                          result)
                        directories)
                      (let*
                        ( (file (first files)) (file-path (string-append path file))
                          (is-dir (directory? file-path)))
                        (if (regexp-exec pattern file)
                          (if (null? (tail parsed))
                            (loop2 (tail files)
                              (if is-dir (pair file-path directories) directories)
                              (if is-dir (pair file-path skip-directories) skip-directories) path
                              (pair file-path result) skip)
                            (loop2 (tail files)
                              (if is-dir (pair file-path directories) directories) skip-directories
                              path result skip))
                          (loop2 (tail files) directories
                            (if is-dir (pair file-path skip-directories) skip-directories) path
                            result skip))))))))))))))

(define*
  (copy-file-recursive source target #:key stop-on-error display-errors (copy-file copy-file)
    (ensure-directory ensure-directory-structure))
  "string:path string:path keyword-options ... -> unspecified
   copy source to target. copies the whole directory structure if source is a directory.
   target must include the first new filename, for example source:/etc/dircolors target:/tmp/dircolors.
   the copy-file procedure {source-path target-path -> boolean} can be replaced, to symlink instead, for example.
   #:stop-on-error boolean
   #:display-errors boolean
   #:copy-file procedure"
  (if (directory? source)
    (let*
      ( (source-prefix (dirname source)) (target-prefix (dirname target))
        (get-target-path
          (l (source) (string-append target-prefix (string-drop-prefix source-prefix source)))))
      (ftw source
        (l (source stat-info flag)
          (case flag
            ((directory) (ensure-directory (get-target-path source)) #t)
            ((regular) (copy-file source (get-target-path source)) #t)
            (else
              (and display-errors
                (display (string-append "error copying file " source "\n") (current-error-port)))
              (not stop-on-error))))))
    (copy-file source target)))

(define (directory? path) "test if path exists and is a directory"
  (eq? (q directory) (stat:type (stat path))))

(define (directory-fold path proc init)
  "string procedure:{string any -> any} any -> any
   fold over entries in directory"
  (let (d (if (string? path) (opendir path) path))
    (let loop ((e (readdir d)) (result init))
      (if (eof-object? e) (begin (closedir d) result) (loop (readdir d) (proc e result))))))

(define (call-with-directory path proc) "string procedure:{directory-port -> any} -> any"
  (let (d (opendir path)) (begin-first (proc d) (closedir d))))

(define directory-list scandir)

(define* (directory-list-full path #:optional (select? identity))
  "string procedure ... -> (string ...)
   get a list of full paths to directory entries"
  (let (path (ensure-trailing-slash path))
    (map (l (a) (path->full-path (string-append path a))) (directory-list path select?))))

(define* (directory-tree path #:key select? enter? (stat stat))
  "string [procedure:{any -> boolean}] -> (string:path ...)
   string procedure -> (string ...)
   results in a list of all paths under path, excluding path and the directory references \".\" and \"..\""
  (file-system-fold (if enter? (l (n s r) (enter? n s)) (const #t))
    (if select? (l (n s r) "leaf" (if (select? n s) (pair n r) r)) (l (n s r) (pair n r)))
    (if select? (l (n s r) "down" (if (select? n s) (pair n r) r)) (l (n s r) (pair n r)))
    (l (n s r) "up" r) (l (n s r) "skip" r)
    (l (n s error r) "error"
      (display (string-append (strerror error) " " n "\n") (current-error-port)) r)
    null path stat))

(define* (directory-tree-leaf-directories start #:key (select? (const #t)) enter? (stat stat))
  "string:path -> (string ...)
   return a list of all directories under start that dont have any directory in their entries"
  (directory-tree start #:enter?
    enter? #:select?
    (l (a stat-info)
      (and (eq? (q directory) (stat:type stat-info)) (= 2 (stat:nlink stat-info))
        (select? a stat-info)))))

(define* (directory-prefix-tree start #:optional (directory-tree directory-tree))
  "-> (string/list ...)
   get directory contents as a prefix list.
   example
   (directory-prefix-tree (list \"/usr/local/bin\" \"/usr/local/lib\"))
   -> (\"/usr\" (\"local\" (\"bin\" \"lib\")))"
  (group-recursively (map path->list (directory-tree start))))

(define (dotfile? name)
  "string -> boolean
   checks if name is non-empty and begins with a dot.
   useful for matching hidden files or the directory references . and .."
  (and (not (string-null? name)) (eqv? (string-ref name 0) #\.)))

(define (ensure-directory-structure path)
  "string -> boolean:exists
   try to create any directories of path that do not exist.
   true if the path exists either because it has been created or otherwise.
   every path part is considered a directory"
  (or (file-exists? path) (begin-first (ensure-directory-structure (dirname path)) (mkdir path))))

(define (ensure-directory-structure-and-new-mode path mode)
  "string -> boolean
   like ensure-directory-structure but also sets the file mode/permissions for new directories.
   the mode is influenced by the umask"
  (or (file-exists? path)
    (begin (ensure-directory-structure-and-new-mode (dirname path) mode) (mkdir path mode))))

(define (ensure-trailing-slash str) "string -> string"
  (if (or (string-null? str) (not (eqv? #\/ (string-ref str (- (string-length str) 1)))))
    (string-append str "/") str))

(define (path-directories a)
  "string -> (string:parent-path ...)
   creates a list of the full paths of all directories above the given path"
  (unfold (l (a) (or (string-equal? "/" a) (string-equal? "." a))) identity dirname a))

(define stat-field-name->stat-accessor-ht
  (ht-create-symbol-q mtime stat:mtime
    atime stat:atime
    size stat:size mode stat:mode uid stat:uid gid stat:gid nlink stat:nlink ctime stat:ctime))

(define (stat-field-name->stat-accessor a)
  "symbol -> guile-stat-accessor
   a guile-stat-accessor is for example stat:mtime, and the argument is as symbol for the part after stat:, in this case mtime.
   utility for functions working with file change events and stat-records"
  (ht-ref stat-field-name->stat-accessor-ht a))

(define stat-accessor->stat-field-name-ht
  (ht-create stat:mtime (q mtime)
    stat:atime (q atime)
    stat:size (q size)
    stat:mode (q mode) stat:uid (q uid) stat:gid (q gid) stat:nlink (q nlink) stat:ctime (q ctime)))

(define (stat-accessor->stat-field-name a)
  "utility for functions working with file change events and stat-records"
  (ht-ref stat-accessor->stat-field-name-ht a))

(define (filename-extension a)
  "string -> string
   results in the last dot-separated part of string or the empty-string if no such part exists"
  (let ((r (string-split a #\.))) (if (< 1 (length r)) (last r) "")))

(define* (get-unique-path path #:optional suffix)
  "string [string] -> string
   if the given path with suffix already exists, insert a string between path and the suffix
   until a path is found that doesnt yet exist. suffix is empty by default.
   may append a period and base32 number.
   examples
     \"/tmp/abc\" -> \"/tmp/abc.1\"
     \"/tmp/abc\" \".scm\" -> \"/tmp/abc.1.scm\""
  (let* ((suffix (or suffix "")) (full-path (string-append path suffix)))
    (if (file-exists? full-path)
      (let (next (l (count) (string-append path "." (number->string count 32) suffix)))
        (let loop ((path (next 1)) (count 1))
          (if (file-exists? path) (loop (next count) (+ 1 count)) path)))
      full-path)))

(define (mtime-difference . paths)
  "string ... -> integer
   get the mtimes for paths and subtract from the first mtime all subsequent.
   at least one file has changed if the number is not zero"
  (apply - (par-map (compose stat:mtime stat) paths)))

(define (remove-trailing-slash a)
  "string -> string
   remove any trailing slashes"
  (string-trim-right a #\/))

(define (path-append first-a . a)
  "string ... -> string
   combine string representations of filesystem paths regardless of leading or trailing slashes of the parts"
  (if (null? a) first-a
    (string-join (pair (string-trim-right first-a #\/) (map (l (a) (string-trim-both a #\/)) a))
      "/")))

(define (path-append* first-a . a)
  "string ... -> string
   like path-append but also removes redundant slashes in the middle of the given parts"
  (if (null? a) first-a
    (string-join
      (pair (string-trim-right first-a #\/)
        (map (l (a) (string-join (delete "" (string-split (string-trim-both a #\/) #\/)) "/")) a))
      "/")))

(define (path->list path)
  "string -> list
   parse a string representation of a filesystem path to a list of its parts.
   an empty string as the first element in the list stands for the root directory.
   removes unnecessary slashes.
   example input/output
     \"/b\" -> (\"\" \"b\")
     \"b\" -> (\"b\")"
  (if (string-null? path) (list)
    (let (result (remove string-null? (string-split path #\/)))
      (if (eqv? (string-ref path 0) #\/) (pair "" result) result))))

(define* (list->path a)
  "(string ...) -> string
   for a full path prepend an empty string to the input. this is analogous to the output of path->list"
  (if (equal? (list "") a) "/" (string-join a "/")))

(define (readlink* path)
  "string -> string
   like readlink but also resolves symlinks to symlinks until a non-symlink is found or a target does not exist"
  (let* ((path (readlink path)) (stat-info (false-if-exception (stat path))))
    (if stat-info (if (eq? (q symlink) (stat:type stat-info)) (readlink* path) path) path)))

(define (realpath* path)
  "string -> false/string
   resolves the directory references \".\" and \"..\" as well as symlinks in the given path and removes unnecessary slashes.
   named realpath* because it does not use the posix realpath because guile currently does not include it.
   the foreign function interface could be an alternative"
  (and-let* ((path (path->full-path path)) (path-list (path->list path)))
    (let loop ((rest (tail path-list)) (result (list "")))
      (if (null? rest) (list->path (reverse result))
        (let (a (first rest))
          (cond
            ((string-equal? "." a) (loop (tail rest) result))
            ((string-equal? ".." a) (loop (tail rest) (tail result)))
            (else
              (let*
                ( (path (list->path (reverse (pair a result))))
                  (stat-info (false-if-exception (lstat path))))
                (and stat-info
                  (if (eq? (q symlink) (stat:type stat-info))
                    (let (link-target (readlink* path))
                      (if (string-prefix? "/" link-target)
                        (loop (append (tail (path->list link-target)) (tail rest)) (list ""))
                        (loop (append (path->list link-target) (tail rest)) result)))
                    (loop (tail rest) (pair a result))))))))))))

(define* (path->full-path path #:optional realpath?)
  "string -> string
   uses \"getcwd\" to complete relative paths.
   with \"getcwd\" the basename can be a symlink but all other parts have symlinks resolved.
   the environment variable PWD is not used because it is not usually updated when the process changes directory"
  (if (string-prefix? "/" path) path (string-append (getcwd) "/" path)))

(define remove-filename-extension
  (let
    ( (remove-filename-extension-one
        (l (extension filename)
          (if (string-suffix? extension filename)
            (string-drop-right filename (string-length extension)) filename))))
    (l* (name #:optional fn-extensions all?)
      "string [(string)] [boolean]-> string\n        remove specific, all or the last filename-extension from a string.\n        filename-extension: period characters-except-period ..."
      (if fn-extensions (fold remove-filename-extension-one name fn-extensions)
        (let (name-split (string-split name #\.))
          (if (null? (tail name-split)) name
            (if all? (first name-split)
              (string-drop-right name (+ 1 (string-length (last name-split)))))))))))

(define* (search-load-path path #:optional (load-paths %load-path))
  "gives the first match of a relative-path in load-paths or false.
   all paths in load-paths must end with a \"/\".
   searches in guiles %load-path by default"
  (any
    (l (base-path)
      (let (full-path (string-append base-path path)) (if (file-exists? full-path) full-path #f)))
    load-paths))

(define (stat-diff->accessors stat-info-a stat-info-b accessors)
  "stat stat (procedure ...) -> (stat-accessor ...)
   find the difference between two guile stat objects.
   filter accessors, stat:mtime for example, for fields which do not differ between two stat objects"
  (filter (l (a) (not (equal? (a stat-info-a) (a stat-info-b)))) accessors))

(define (stat-diff stat-info-a stat-info-b accessors)
  "stat stat (procedure ...) -> (#(accessor field-value-a field-value-b)/#f ...)
   find the difference between two guile stat objects.
   map accessors, stat:mtime for example, to vectors for fields which differ between two stat objects"
  (filter-map
    (l (a)
      (let ((value-a (a stat-info-a)) (value-b (a stat-info-b)))
        (if (equal? value-a value-b) #f (vector a value-a value-b))))
    accessors))

(define* (symbol-path->string symbol-path #:optional base-path) "symbol/(symbol ...) -> string"
  (let
    ( (path
        (if (list? symbol-path) (string-join (map symbol->string symbol-path) "/")
          (if (symbol? symbol-path) (symbol->string symbol-path)
            (if (string? symbol-path) symbol-path #f)))))
    (if base-path (string-append (ensure-trailing-slash base-path) path) path)))

(define (symbol-path? a) (and (list? a) (not (null? a)) (symbol? (first a))))
(define* (symbol-path->path a) (string-join (map symbol->string a) "/"))

(define* (poll-watch paths events proc min-interval #:optional (max-interval min-interval))
  "(string ...) (symbol ...) {diff file-descriptors stat-info -> unspecified} milliseconds [milliseconds] -> unspecified
   observe stat information of multiple files (which can be directories)
   by checking for events of change (which are specified as names of stat object accessors, for example stat:mtime, without the stat: prefix)
   and call proc if any of those changes have occurred. the diff passed to proc is a result of stat-diff.
   the files are checked in intervals with sizes between min-interval and max-interval,
   automatically adjusting the interval size to match change frequency"
  (let ((accessors (map stat-field-name->stat-accessor events)))
    (let
      ( (fdes (map (l (e) (open e O_RDONLY)) (any->list paths)))
        (max-interval (* max-interval 1000)) (min-interval (* min-interval 1000))
        (stat-diff* (l (si-1 si-2) (stat-diff->accessors si-1 si-2 accessors))))
      (call-at-approximated-interval
        (l (prev-stat-info)
          (let* ((stat-info (map stat fdes)) (diff (map stat-diff* prev-stat-info stat-info)))
            (if (every null? diff) (list -1 stat-info)
              (begin (proc diff fdes stat-info) (list 1 stat-info)))))
        min-interval max-interval 1.1 0.2 (map stat fdes)))))

(define (directory-reference? file-path)
  "string -> boolean
   test if given string designates a directory reference, either \".\" or \"..\"
   can be used as a filter to directory-listing procedures."
  (let ((name (basename file-path))) (if (or (string= name ".") (string= name "..")) #t #f)))

(define* (fold-directory-tree proc init path #:optional (max-depth (inf)))
  "::
   procedure:{string:current-path guile-stat-object:stat-info any:previous-result -> any} any string [integer] {string/path -> boolean} ...
   ->
   any:last-procedure-result

   *deprecated* in favor of (ice-9 ftw) filesystem-fold.
   fold over directory-tree under path, possibly limited by max-depth.
   the directory-references \".\" and \"..\" are ignored.
   call to proc is (proc full-path stat-info previous-result/init)"
  (let (path (ensure-trailing-slash path))
    (fold
      (l (e r)
        (let* ((full-path (string-append path e)) (stat-info (stat full-path)))
          (if (and (eq? (q directory) (stat:type stat-info)) (< 1 max-depth))
            (fold-directory-tree proc (proc full-path stat-info r)
              (string-append full-path "/") (- max-depth 1))
            (proc full-path stat-info r))))
      init (directory-list path (negate directory-reference?)))))

(define* (directory-tree-each proc path #:optional (max-depth (inf)))
  "procedure:{string stat-object -> unspecified} string [integer] -> unspecified"
  (fold-directory-tree (l (path stat-info r) (proc path stat-info)) #f path max-depth))
