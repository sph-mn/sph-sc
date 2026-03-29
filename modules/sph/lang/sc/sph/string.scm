(define-module (sph lang sc sph string))

(use-modules (ice-9 pretty-print) (ice-9 regex)
  (sph lang sc sph) (srfi srfi-1) ((rnrs bytevectors) #:select (u8-list->bytevector utf8->string)))

(export any->string any->string-display
  any->string-pretty-print any->string-write
  any->string-write* list->string-columns
  list-string-append-each parenthesize
  parenthesized? parenthesise
  parenthesised? regexp-match-replace
  regexp-replace sph-string-description
  string-ascii->utf8 string-brackets-closed?
  string-brackets-unclosed-count string-camelcase->dash
  string-camelcase->underscore string-case
  string-compress-space string-contains-all?
  string-contains-char? string-contains-some?
  string-downcase-first string-drop-prefix
  string-drop-prefix-if-exists string-drop-suffix
  string-drop-suffix-if-exists string-each
  string-enclose string-equal?
  string-fill-left string-fill-right
  string-indices string-indices-char
  string-join-tree string-last-index
  string-longest-prefix string-lowercase?
  string-matches-any-regexp? string-matches-every-regexp?
  string-multiply string-numeric?
  string-octet-length string-quote
  string-replace-char string-replace-chars
  string-replace-string string-replace-strings
  string-skip-string string-slice-at-words string-split-regexp string-trim-string symbol?->string)

(define sph-string-description
  "string processing helpers.
   # highlights
   string-replace-string: a fast replacer
   string-brackets-enclosed?: check that a string contains correctly nested and balanced count of round brackets or any other pair of start/end characters
   any->string
   # syntax
   string-case :: string (condition any:consequent) ...
     like case but for strings instead of symbols.
     example:
       (string-case \"test\"
         (\"a\" 1)
         ((\"b\" \"c\") 2)
         (mystringlist 3)
         (else 4))")

(define (fold-multiple f a . custom-state-values)
  (if (null? a) custom-state-values
    (apply fold-multiple f (tail a) (apply f (first a) custom-state-values))))

; backward compatibility
(define parenthesise parenthesize)
(define parenthesised? parenthesized?)

(define-syntax-case (string-case a (condition expr) ...) s
  (let*
    ( (b (syntax->datum (syntax ((condition expr) ...)))) (c (gensym "string-case-"))
      (cond-datum
        (pair (q cond)
          (map
            (l (a) "(else x) is passed directly to the parent cond"
              (let ((a-first (first a)) (a-tail (tail a)))
                (cond
                  ((string? a-first) (pair (list (q string-equal?) a-first c) a-tail))
                  ((list? a-first) (pair (list (q member) c (list (q quote) a-first)) a-tail))
                  ( (and (symbol? a-first) (not (eq? (q else) a-first)))
                    (pair (list (q member) c a-first) a-tail))
                  (else a))))
            b))))
    (quasisyntax ((unsyntax (datum->syntax s (qq (lambda ((unquote c)) (unquote cond-datum))))) a))))

(define (string-camelcase-replace a replace-proc)
  "string {match-structure -> replacement} -> string"
  (regexp-replace a "(\\s|^)[a-z][a-zA-Z0-9]+"
    (l (match) (regexp-replace (match:substring match) "[A-Z]" replace-proc))))

(define* (string-list-append-each a before #:optional after)
  "(string ...) string/boolean-false [string/boolean-false] -> (string ...)
   append strings before and after respectively to every element of string list a.
   may return a unmodified"
  (if before
    (map (if after (l (e) (string-append before e after)) (l (e) (string-append before e))) a)
    (if after (map (l (e) (string-append e after)) a) a)))

(define (any->string a)
  "any -> string
   generalized string conversion function.
   get the string representation for the value of an object.
   symbols like \".\" are converted to \"#{.}\" using display."
  (if (string? a) a
    (if (symbol? a) (symbol->string a) (call-with-output-string (l (port) (display a port))))))

(define (any->string-display a) (object->string a display))
(define (any->string-pretty-print a) (object->string a pretty-print))
(define (any->string-write a) (object->string a write))

(define (any->string-write* a)
  "write converts symbols like \".\" to #{.}, this procedure avoids this"
  (if (symbol? a) (symbol->string a) (any->string-write a)))

(define* (list->string-columns a #:key (justify (q left)) (separator "") (max-width 78))
  "(string ...) #:justify symbol:left/right #:separator string #:max-width integer -> string
   create a string of equally sized columns containing elements of the string list \"a\", separated
   by #:separator"
  (string-join
    (map
      (let
        ( (column-width (inexact->exact (round (/ max-width (length a)))))
          (justify (if (eqv? (q left) justify) string-pad-right string-pad)))
        (l (e) (justify e column-width)))
      a)
    separator))

(define (parenthesize a)
  "string -> string
   surround string with an open and a closing round bracket"
  (string-append "(" a ")"))

(define (parenthesized? a)
  "string -> boolean
   checks if the string is enclosed by round brackets.
   also checks if every opening bracket is paired with a closing one after it"
  (string-brackets-enclosed? a #\( #\)))

(define (regexp-match-replace a replacements)
  "string ((regexp . string:replacement)/(regexp string:search-string . string:replacement) ...) -> string
   replace strings inside string portions matched by regular expressions
   the two part replacement element makes a simple replace,
   the three part version replaces search-string only inside matches with replacement"
  (fold
    (l (a result)
      (fold-matches (first a) result
        result
        (l (match result)
          (if (pair? (tail a))
            (string-replace-string result (match:substring match)
              (string-replace-string (match:substring match) (first (tail a)) (tail (tail a))))
            (string-replace-string result (match:substring match) (tail a))))))
    a replacements))

(define (regexp-replace str regexp replacement)
  "string string/regexp string/procedure:{match-structure -> string} -> string
   replace all occurences of regexp in string with replacement"
  (regexp-substitute/global #f regexp str (q pre) replacement (q post)))

(define (string-ascii->utf8 a)
  "string -> string
   convert an ascii string that has incorrectly coded utf8 chars to an utf8 string"
  (utf8->string (u8-list->bytevector (map char->integer (string->list a)))))

(define (string-brackets-unclosed-count a start-char end-char)
  "string character character -> integer:unclosed-count"
  (string-fold
    (l (a result) (+ result (cond ((eqv? start-char a) 1) ((eqv? end-char a) -1) (else 0)))) 0 a))

(define (string-brackets-closed? a start-char end-char)
  "string character character -> boolean
   checks if every start-char in string has an end-char somewhere after it.
   this can be used to check if there are missing round brackets in a string.
   example: (string-brackets-closed? \"(ab (cd (e)) f)\" #\\( #\\))"
  (= 0 (string-brackets-unclosed-count a start-char end-char)))

(define (string-brackets-enclosed? a start-char end-char)
  "string character character -> boolean
   check if string has start-char as the first character and end-char as the last, and if
   occurrences of start-char and end-char as opening and closing brackets are balanced"
  (let (len (string-length a))
    (and (< 1 len) (eq? start-char (string-ref a 0))
      (eq? end-char (string-ref a (- len 1)))
      (let loop ((index 1) (depth 1))
        (cond
          ((= depth 0) (= len index))
          ( (< index len)
            (loop (+ 1 index)
              (cond
                ((eqv? start-char (string-ref a index)) (+ depth 1))
                ((eqv? end-char (string-ref a index)) (- depth 1))
                (else depth))))
          (else #f))))))

(define (string-camelcase->dash a)
  "string -> string
   aA -> a-a
   aa aAa -> aa a-aa"
  (string-camelcase-replace a
    (l (match) (string-append "-" (string-downcase (match:substring match))))))

(define (string-camelcase->underscore a)
  "string -> string
   aA -> a-a
   aa aAa -> aa a-aa"
  (string-camelcase-replace a
    (l (match) (string-append "_" (string-downcase (match:substring match))))))

(define (string-compress-space a)
  "string -> string
   replace multiple subsequent space characters with one space"
  (regexp-replace a " {2,}" " "))

(define (string-contains-all? a b)
  "string (string ...) -> boolean
   result in a boolean indicating if string contains all of the patterns"
  (every (l (b) (string-contains a b)) b))

(define (string-contains-char? a char) "string character -> boolean"
  (if (string-index a char) #t #f))

(define (string-contains-some? a b)
  "string (string ...) -> boolean
   result in a boolean indicating if string contains any of the patterns"
  (any (l (b) (string-contains a b)) b))

(define (string-downcase-first a)
  "string -> string
   AA -> aA
   Aa -> aa"
  (if (string-null? a) a
    (let (a (string->list a)) (list->string (pair (char-downcase (first a)) (tail a))))))

(define (string-drop-prefix prefix a) "string string -> string"
  (string-drop a (string-length prefix)))

(define (string-drop-prefix-if-exists prefix a)
  "string string -> string
   remove prefix if string has prefix"
  (if (string-prefix? prefix a) (string-drop-prefix prefix a) a))

(define (string-drop-suffix suffix a) "string string -> string"
  (string-drop-right a (string-length suffix)))

(define (string-drop-suffix-if-exists suffix a)
  "string string -> string
   remove suffix if string has suffix"
  (if (string-suffix? suffix a) (string-drop-suffix suffix a) a))

(define string-each string-for-each)

(define (string-enclose a enclose-str) "append enclose-str to beginning and end of argument string"
  (string-append enclose-str a enclose-str))

(define string-equal? string=)

(define (string-fill-left a target-length character)
  "string character integer -> string
   prepend character to the given string until the string length equals target-length.
   examples
     (string-fill-left \"1\" #\\0 2) -> \"01\"
     (string-fill-left \"10\" #\\0 2) -> \"10\"
   string-fill-left-zeros"
  (let (count (- target-length (string-length a)))
    (if (> count 0) (string-append (list->string (make-list count character)) a) a)))

(define (string-fill-right a target-length character)
  (let (count (- target-length (string-length a)))
    (if (> count 0) (string-append a (list->string (make-list count character))) a)))

(define (string-indices a search-string)
  "string string -> (integer ...)
   result in a list of indices at which search-string occurs in a string"
  (let ((search-string-length (string-length search-string)) (a-length (string-length a)))
    (let loop ((index (string-contains a search-string)))
      (if index
        (if (= index a-length) (list index)
          (pair index
            (loop (string-contains a search-string (+ index (max 1 search-string-length))))))
        (list)))))

(define (string-indices-char a search-char)
  "string char -> (integer ...)
   create a list of indices at which search-char occurs in a string"
  (let (a-length (string-length a))
    (let loop ((index 0))
      (if (< index a-length)
        (if (eqv? search-char (string-ref a index)) (pair index (loop (+ 1 index)))
          (loop (+ 1 index)))
        (list)))))

(define (string-join-tree a delimiter)
  "(list/string ...) string -> string
   same as (string-join (flatten a) delimiter)"
  (string-join (map (l (e) (if (list? e) (string-join-tree e delimiter) e)) a) delimiter))

(define (string-last-index a)
  "string -> integer
   get the last possible index of a string"
  (if (string-null? a) 0 (- (string-length a) 1)))

(define (string-longest-prefix a prefix-list)
  "string (string ...) -> string/boolean
   result in the element of prefix-list that is the longest prefix of prefix-list in string \"a\""
  (fold
    (l (e prev)
      (if (string-prefix? e a) (if prev (if (> (string-length e) (string-length prev)) e prev) e)
        prev))
    #f prefix-list))

(define (string-lowercase? a) "test if a string contains no uppercase characters"
  (not (string-any (l (c) (eqv? (char-general-category c) (q Lu))) a)))

(define (string-matches-any-regexp? a regexp-list) "string (regexp-object ...) -> boolean"
  (any (l (e) (regexp-exec e a)) regexp-list))

(define (string-matches-every-regexp? a regexp-list) "string (regexp-object ...) -> boolean"
  (every (l (e) (regexp-exec e a)) regexp-list))

(define (string-multiply a n) "string integer -> string" (apply string-append (make-list n a)))

(define (string-numeric? a)
  "string -> boolean
   check if string is a valid scheme representation of a number"
  (if (string->number a) #t #f))

(define (string-octet-length a)
  "string -> integer
   the number of bytes of string, regardless of the character encoding, without terminator like \"null\""
  (* (string-bytes-per-char a) (string-length a)))

(define (string-quote a) "string -> string"
  "enclose a string with \" or ' quotes, depending on if the string already\n   includes one of these. preferring \". returns false if string already contains both types of quotes"
  (if (string-contains a "\"") (if (string-contains a "'") #f (string-enclose a "'"))
    (string-enclose a "\"")))

(define (string-replace-char a char replacement)
  "string character character -> string
   replace all occurences of \"char\" in a string"
  (string-map (l (e) (if (eqv? e char) replacement e)) a))

(define (string-replace-chars a spec)
  "string ((char [replacements] ...) ...) -> string
   replace chars in string with none, one or multiple chars"
  (list->string
    (string-fold-right
      (l (e prev)
        (let ((info (assoc e spec)))
          (if (and info (not (null? info))) (append (tail info) prev) (pair e prev))))
      (list) a)))

(define (string-replace-string a replace replacement)
  "string string string -> string
   replace all occurences of string \"replace\" inside string \"a\" with string \"replacement\".
   tests have shown it to be up to 28x times faster than regexp-substitute/global (guile2.06 22x, guile3 5.75-28x).
   this procedure is quite nice to debug - comment out one or all string-copy! calls and
   the result string will be a partial result"
  (let (indices (string-indices a replace))
    (if (null? indices) a
      (let
        ( (replace-length (string-length replace)) (replacement-length (string-length replacement))
          (a-length (string-length a)))
        "calculate result string size and create result string"
        (let ((r-length (+ a-length (* (length indices) (- replacement-length replace-length)))))
          (let (r (make-string r-length))
            "each match index, copy characters before match-end to the result string"
            (let loop
              ((r-index 0) (prev-index 0) (cur-index (first indices)) (rest (tail indices)))
              (string-copy! r r-index a prev-index cur-index)
              (let (r-index (- r-index (- prev-index cur-index)))
                (string-copy! r r-index replacement)
                (if (null? rest)
                  (begin
                    (if (< (+ cur-index replace-length) a-length)
                      (string-copy! r (+ r-index replacement-length)
                        a (+ cur-index replace-length) a-length))
                    r)
                  (loop (+ r-index replacement-length) (+ cur-index replace-length)
                    (first rest) (tail rest)))))))))))

(define (string-replace-strings a strings-and-replacements)
  "string ((string:search . string:replacement) ...) -> string"
  (fold (l (b result) (string-replace-string result (first b) (tail b))) a strings-and-replacements))

(define (string-skip-string a skip)
  "string string -> integer
   skip over string \"skip\" at the beginning of a string and return the first index afterwards,
   or the first index 0 if skip string is not a prefix"
  (let (skip-length (string-length skip))
    (let loop ((r #f) (prev (string-contains a skip)))
      (if (and prev (or (not r) (= (- prev skip-length) r)))
        (loop prev (string-contains a skip (+ skip-length prev)))
        (if r (min (- (string-length a) 1) (+ 1 skip-length r)) r)))))

(define (string-slice-at-words a slice-length)
  "string integer -> (string ...)
   split line into slices/chunks of size slice-length, unless it would split words (subsequent characters without the space character),
   in which case the word is moved to the next slice. ignores newlines. can be used for single lines. can be used for \"text-wrapping\".
   splits string at spaces, then uses the parts to build lines while checking if the line length with spaces would exceed slice-length.
   if yes, the exceeding part is moved to the next line"
  (let
    ( (words (string-split a #\space))
      (prepend-line (l (e r) (pair (string-join (reverse e) " ") r))))
    (let
      (r
        (fold-multiple
          (l (word-length words line line-spaces-length current-slice-length r)
            (if (> (+ word-length line-spaces-length current-slice-length) slice-length)
              (list (tail words) (list (first words)) 0 word-length (prepend-line line r))
              (list (tail words) (pair (first words) line)
                (+ 1 line-spaces-length) (+ word-length current-slice-length) r)))
          (map string-length words) words (list) 0 0 (list)))
      (apply
        (l (words line line-spaces-length current-slice-length r) (reverse (prepend-line line r))) r))))

(define* (string-split-regexp str regexp #:optional (handle-delim (q discard)))
  "string string [symbol:discard/concat] -> (string ...)
   split string into a list of substrings delimited by a regular expression.
   1. all regexp matches are collected
   2. substrings between matches are extracted"
  (let (matches (list-matches regexp str))
    (if (null? matches) (list str)
      (call-with-values
        (nullary
          (if (eqv? (q discard) handle-delim)
            (values (l (e prev) (substring str (match:end prev) (match:start e)))
              (substring str 0 (match:start (first matches))))
            (if (eqv? (q concat) handle-delim)
              (values (l (e prev) (substring str (match:end prev) (match:end e)))
                (substring str 0 (match:end (first matches))))
              (throw (q wrong-argument-for-handle-delim) handle-delim))))
        (l (get-substring init-parts)
          (let loop ((rest (tail matches)) (parts (list init-parts)) (prev (first matches)))
            (if (null? rest) (reverse (pair (substring str (match:end prev)) parts))
              (loop (tail rest) (pair (get-substring (first rest) prev) parts) (first rest)))))))))

(define (string-trim-string a trim)
  "string string -> string
   remove all occurences of string \"trim\" from the beginning of a string.
   like string-trim but with a trim-string instead of a character"
  (let (skip-index (string-skip-string a trim))
    (if skip-index (string-drop a (if (= 0 skip-index) skip-index (- skip-index 1))) a)))

(define (symbol?->string a)
  "any -> any
   converts \"a\" to string only if it is a symbol, otherwise results in \"a\""
  (if (symbol? a) (symbol->string a) a))
