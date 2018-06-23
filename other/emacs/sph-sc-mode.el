; sph-sc.el - emacs mode for sph-sc
; version 2018-05-20
; Copyright (C) 2018 sph http://sph.mn <sph@posteo.eu> (current maintainer)

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <http://www.gnu.org/licenses/>.

(defalias 'pointto-toplevel-sexp-begin 'beginning-of-defun)
(defalias 'sequence-ref 'elt)

;-- syntax --;
; the syntax-table describes how various standard functions will treat the text, for example movement within the buffer with 'forward-word'
(defvar scheme-mode-syntax-table
  (let ((st (make-syntax-table)) (i 0))
    (while (< i ?0)
      (modify-syntax-entry i "_ " st)
      (setq i (1+ i)))
    (setq i (1+ ?9))
    (while (< i ?A)
      (modify-syntax-entry i "_ " st)
      (setq i (1+ i)))
    (setq i (1+ ?Z))
    (while (< i ?a)
      (modify-syntax-entry i "_ " st)
      (setq i (1+ i)))
    (setq i (1+ ?z))
    (while (< i 128)
      (modify-syntax-entry i "_ " st)
      (setq i (1+ i)))
    (modify-syntax-entry ?\t " " st)
    (modify-syntax-entry ?\n "> " st)
    (modify-syntax-entry ?\f " " st)
    (modify-syntax-entry ?\r " " st)
    (modify-syntax-entry ?\s " " st)
    ;currently incompatible with lisp-font-lock-syntactic-face-function
    ;(modify-syntax-entry ?\| "\" 23bn" st)
    (modify-syntax-entry ?\( "() " st)
    (modify-syntax-entry ?\) ")( " st)
    (modify-syntax-entry ?\; "< 2 " st)
    (modify-syntax-entry ?\" "\" " st)
    (modify-syntax-entry ?' "' " st)
    (modify-syntax-entry ?` "' " st)
    (modify-syntax-entry ?, "' " st)
    (modify-syntax-entry ?@ "' " st)
    (modify-syntax-entry ?# "' 14" st)
    (modify-syntax-entry ?\\ "\\ " st)
    st))

;-- syntax highlighting --;

(defconst sph-sc-sexp-comment-syntax-table
  (let ((st (make-syntax-table scheme-mode-syntax-table)))
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?\n " " st)
    (modify-syntax-entry ?# "'" st)
    st))

(defun lisp-font-lock-syntactic-face-function (state)
  (if (nth 3 state)
    ;This might be a (doc) string or a ... symbol.
    (let ((startpos (nth 8 state)))
      (if (eq (char-after startpos) ?|)
        ;This is not a string, but a ... symbol.
        nil
        (let* ((listbeg (nth 1 state))
            (firstsym (and listbeg
                (save-excursion
                  (goto-char listbeg)
                  (and (looking-at "([ \t\n]*\\(\\(\\sw\\|\\s_\\)+\\)")
                    (match-string 1)))))
            (docelt (and firstsym
                (function-get (intern-soft firstsym)
                  lisp-doc-string-elt-property))))
          (if (and docelt
              ;; It's a string in a form that can have a docstring.
              ;; Check whether it's in docstring position.
              (save-excursion
                (when (functionp docelt)
                  (goto-char (match-end 1))
                  (setq docelt (funcall docelt)))
                (goto-char listbeg)
                (forward-char 1)
                (condition-case nil
                  (while (and (> docelt 0) (< (point) startpos)
                      (progn (forward-sexp 1) t))
                    (setq docelt (1- docelt)))
                  (error nil))
                (and (zerop docelt) (<= (point) startpos)
                  (progn (forward-comment (point-max)) t)
                  (= (point) (nth 8 state)))))
            font-lock-doc-face
            font-lock-string-face))))
    font-lock-comment-face))

(defun sph-sc-font-lock-syntactic-face-function (state)
  ; for highlighting srfi-62-comments #;()
  (when (and (null (nth 3 state))
      (eq (char-after (nth 8 state)) ?#)
      (eq (char-after (1+ (nth 8 state))) ?\;))
    (save-excursion
      (let ((pos (point))
          (end
            (condition-case err
              (let ((parse-sexp-lookup-properties nil))
                (goto-char (+ 2 (nth 8 state)))
                ;; this doesn't handle the case where the sexp
                ;; itself contains a #; comment.
                (forward-sexp 1)
                (point))
              (scan-error (nth 2 err)))))
        (when (< pos (- end 2))
          (put-text-property pos (- end 2) 'syntax-table sph-sc-sexp-comment-syntax-table))
        (put-text-property (- end 1) end 'syntax-table '(12)))))
  (lisp-font-lock-syntactic-face-function state))

;the levels are a font-lock feature to loosely set what should be highlighted
(defconst sph-sc-font-lock-keywords-1
  (eval-when-compile
    '("(\\(define[^ ]*?\\) +(\\(\\sw+\\)" (1 'font-lock-keyword-face) (2 'font-lock-function-name-face))))

(defconst sph-sc-font-lock-keywords-2
  (eval-when-compile
    (list
      ; '(font-lock-toplevel-variables (2 'font-lock-variable-name-face) )
      (quote ("(\\(pre-define[^ ]?\\) (\\(\\sw+\\)" (2 font-lock-function-name-face)))
      (quote ("(\\(pre-\\w+\\)" (1 font-lock-preprocessor-face)))
      (quote ("(\\(sc-\\w+\\)" (1 font-lock-preprocessor-face)))
      (quote ("(\\(define[^ ]*?\\) +(\\(\\sw+\\) \\(.*?\\))" (2 font-lock-function-name-face)))
      ; color round brackets
      ;(quote ("(\\|)" . font-lock-string-face))
      ;(quote ("(\\(define[^? ]*?\\) +\\(\\sw+\\)" (2 font-lock-variable-name-face)))
      (quote ("(\\(label\\) +\\(\\sw+\\)" (2 font-lock-function-name-face)))
      (list
        (concat
          "(\\("
          (regexp-opt
            (quote ("begin" "declare" "if" "if*" "goto" "return" "set"))
            t)
          "\\)\\>")
        1 (quote font-lock-keyword-face))
      (list
        (concat
          "(\\("
          (regexp-opt
            (quote ("debug-log" "malloc" "calloc" "realloc" "free"))
            t)
          "\\)\\>")
        1 (quote font-lock-builtin-face))
      '("\\<#t\\|#f\\>" . font-lock-constant-face)
      '("#\\\\[^ ]" . font-lock-constant-face)
      '("\\<\\(\\+\\|-\\)?[0-9]+\\(\\.[0-9]+\\)?" . font-lock-constant-face)
)))

(defvar sph-sc-font-lock-keywords sph-sc-font-lock-keywords-1 "default expressions to highlight in scheme modes")

;-- indentation --;
(defvar compress-whitespace-on-indent t)

(defun sph-sc-indent-line ()
  ; restore-cursor-position makes for the effect of either jumping to the indented beginning of the line,
  ; or letting the cursor move with the line
  (interactive "P")
  (let
    ( (restore-cursor-position (> (current-column) (current-indentation)))
      (indent (save-excursion (sph-sc-calculate-indentation))))
    (if restore-cursor-position
      (save-excursion (indent-line-to indent))
      (indent-line-to indent))
    (if compress-whitespace-on-indent
      (sph-sc-compress-whitespace-on-line indent))))

(defun sph-sc-calculate-indentation ()
  "return the column to which the current line should be indented.
  it finds the current sexp-depth and indents by (* sexp-depth lisp-body-indent)"
  (let ((indent-line-beginning (line-beginning-position)))
    (pointto-toplevel-sexp-begin)
    (if (>= (point) indent-line-beginning) ;depth 1 begins on the current line
      0
      ;indent-point-depth - parse until indent-line-beginning is reached while counting expression nesting-depth
      (* (max 0 (sequence-ref (parse-partial-sexp (point) indent-line-beginning) 0)) lisp-body-indent))))

(defun sph-sc-compress-whitespace-on-line (start)
  ; this compresses whitespace in strings ...
  (save-excursion
    (move-to-column start)
    (while (re-search-forward "[ \t]+" (line-end-position) t)
      (replace-match " "))
    (move-to-column start)
    (while (re-search-forward "[ \t]$" (line-end-position) t)
      (replace-match ""))))

;-- key binding --;
(defvar sph-sc-mode-map
  (let ((keymap (make-sparse-keymap "sph-sc")))
    keymap)
  "keymap for sph-sc-mode")

;-- mode initialisation --;
(defcustom sph-sc-mode-hook nil "normal hook run when entering 'sph-sc-mode'. see 'run-hooks'"
  :type 'hook :group 'scheme)

(defun sph-sc-mode ()
  "major mode for editing scheme code"
  (interactive)
  ;has the effect of switching to fundamental mode and erasing most of the effects of the previous major mode
  (kill-all-local-variables)

  (setq major-mode 'sph-sc-mode)
  (setq mode-name "sph-sc")
  (use-local-map sph-sc-mode-map)
  (set-syntax-table scheme-mode-syntax-table)

  (set (make-local-variable 'parse-sexp-ignore-comments) t)

  ;exposes the indentation function
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'sph-sc-indent-line)

  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-add) 1)

  (set (make-local-variable 'comment-add) 1)
  (make-local-variable 'comment-start-skip)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (set (make-local-variable 'comment-start-skip) "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+[ \t]*")
  (set (make-local-variable 'font-lock-comment-start-skip) ";+ *")
  (set (make-local-variable 'comment-column) 40)

  (set
    (make-local-variable 'font-lock-defaults)
    '( (sph-sc-font-lock-keywords sph-sc-font-lock-keywords-1 sph-sc-font-lock-keywords-2)
      nil t (("+-*/.<>=!?$%_&~^:" . "w") (?#. "w 14"))
      pointto-toplevel-sexp-begin
      (font-lock-mark-block-function . mark-defun)
      (font-lock-syntactic-face-function . sph-sc-font-lock-syntactic-face-function)
      (parse-sexp-lookup-properties . t)
      (font-lock-extra-managed-props syntax-table)))
  (run-mode-hooks 'sph-sc-mode-hook))
(defgroup scheme nil
  "editing scheme code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'lisp)
(provide (quote sph-sc-mode))
