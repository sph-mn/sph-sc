;;; sph-sc-mode.el -*- lexical-binding: t; -*-
; version 2025-10-03

(defgroup sph-sc nil "Scheme-like s-expr editing" :group 'languages)
(defcustom sph-sc-compress-on-indent t "Compress spaces on indent/newline outside strings/comments." :type 'boolean :group 'sph-sc)
(defcustom sph-sc-highlight-brackets t "Highlight () with sph-sc-bracket-face." :type 'boolean :group 'sph-sc)
(defface sph-sc-bracket-face '((t :inherit default)) "Face for ()" :group 'sph-sc)
(defalias 'sequence-ref 'elt)

(defvar sph-sc-mode-syntax-table
  (let ((st (make-syntax-table)) (i 0))
    (while (< i ?0) (modify-syntax-entry i "_ " st) (setq i (1+ i)))
    (setq i (1+ ?9))
    (while (< i ?A) (modify-syntax-entry i "_ " st) (setq i (1+ i)))
    (setq i (1+ ?Z))
    (while (< i ?a) (modify-syntax-entry i "_ " st) (setq i (1+ i)))
    (setq i (1+ ?z))
    (while (< i 128) (modify-syntax-entry i "_ " st) (setq i (1+ i)))
    (modify-syntax-entry ?\t " " st)
    (modify-syntax-entry ?\n "> " st)
    (modify-syntax-entry ?\f " " st)
    (modify-syntax-entry ?\r " " st)
    (modify-syntax-entry ?\s " " st)
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

(defconst sph-sc-sexp-comment-syntax-table
  (let ((st (make-syntax-table sph-sc-mode-syntax-table)))
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?\n " " st)
    (modify-syntax-entry ?# "'" st)
    st))

(defun sph-sc--lfl-sff (state)
  (if (nth 3 state)
    (let ((startpos (nth 8 state)))
      (if (eq (char-after startpos) ?|)
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
  (when (and (null (nth 3 state))
      (eq (char-after (nth 8 state)) ?#)
      (eq (char-after (1+ (nth 8 state))) ?\;))
    (save-excursion
      (let ((pos (point))
          (end (condition-case err
              (let ((parse-sexp-lookup-properties nil))
                (goto-char (+ 2 (nth 8 state)))
                (forward-sexp 1)
                (point))
              (scan-error (nth 2 err)))))
        (when (< pos (- end 2))
          (put-text-property pos (- end 2) 'syntax-table sph-sc-sexp-comment-syntax-table))
        (put-text-property (- end 1) end 'syntax-table '(12)))))
  (sph-sc--lfl-sff state))

(defconst sph-sc-font-lock-keywords-1
  (list
    '("(\\(define[^ ]*?\\) +(\\(\\sw+\\)"
      (1 font-lock-keyword-face) (2 font-lock-function-name-face))
    '("[()]" 0 'sph-sc-bracket-face prepend)))

(defconst sph-sc-font-lock-keywords-2
  `(
    ("(\\(pre-define[^ ]?\\) (\\(\\sw+\\)" (2 font-lock-function-name-face))
    ("(\\(pre-\\w+\\)" (1 font-lock-preprocessor-face))
    ("(\\(sc-\\w+\\)" (1 font-lock-preprocessor-face))
    ("(\\(define[^ ]*?\\) +(\\(\\sw+\\) \\(.*?\\))" (2 font-lock-function-name-face))
    ("[()]" 0 'sph-sc-bracket-face prepend)
    ("(\\(define[^? ]*?\\) +\\(\\sw+\\)" (2 font-lock-variable-name-face))
    ("(\\(label\\) +\\(\\sw+\\)" (2 font-lock-function-name-face))
    (,(concat "(\\(" (regexp-opt '("debug-log" "set" "declare") t) "\\)\\>") 1 'font-lock-builtin-face)
    ("\\_<#\\(t\\|f\\)\\_>" . font-lock-constant-face)
    ("#\\\\[^ ]" . font-lock-constant-face)
    ("\\<\\(\\+\\|-\\)?[0-9]+\\(\\.[0-9]+\\)?" . font-lock-constant-face)))

(defvar sph-sc-font-lock-keywords sph-sc-font-lock-keywords-1)

(defvar-local sph-sc--indent-width nil)

(defun sph-sc--ppss-depth-at-bol () (save-excursion (goto-char (line-beginning-position)) (car (syntax-ppss))))

(defun sph-sc--line-compress (start)
  (save-excursion
    (move-to-column start)
    (while (re-search-forward "[ \t]+" (line-end-position) t) (replace-match " "))
    (move-to-column start)
    (while (re-search-forward "[ \t]$" (line-end-position) t) (replace-match ""))))

(defun sph-sc-indent-line ()
  (interactive "P")
  (let* ((restore (> (current-column) (current-indentation)))
      (depth (save-excursion
          (let ((indent-line-beginning (line-beginning-position)))
            (beginning-of-defun)
            (if (>= (point) indent-line-beginning)
              0
              (sequence-ref (parse-partial-sexp (point) indent-line-beginning) 0)))))
      (w (or sph-sc--indent-width lisp-body-indent))
      (want (* (max 0 depth) w)))
    (if restore (save-excursion (indent-line-to want)) (indent-line-to want))
    (when sph-sc-compress-on-indent (sph-sc--line-compress want))))

(defun sph-sc-newline-and-indent () (interactive) (newline) (sph-sc-indent-line))
(defvar sph-sc-mode-map (let ((m (make-sparse-keymap))) (define-key m (kbd "RET") 'sph-sc-newline-and-indent) m))

;;;###autoload
(define-derived-mode sph-sc-mode prog-mode "sph-sc"
  :syntax-table sph-sc-mode-syntax-table
  :keymap sph-sc-mode-map
  (setq-local parse-sexp-ignore-comments t)
  (setq-local indent-line-function #'sph-sc-indent-line)
  (setq-local comment-start ";")
  (setq-local comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+[ \t]*")
  (setq-local comment-add 1)
  (setq-local comment-column 40)
  (setq-local sph-sc--indent-width lisp-body-indent)
  (setq-local font-lock-defaults
    '((sph-sc-font-lock-keywords sph-sc-font-lock-keywords-1 sph-sc-font-lock-keywords-2)
      nil t
      (("+-*/.<>=!?$%_&~^:" . "w") (?# . "w 14"))
      beginning-of-defun
      (font-lock-mark-block-function . mark-defun)
      (font-lock-syntactic-face-function . sph-sc-font-lock-syntactic-face-function)
      (parse-sexp-lookup-properties . t)
      (font-lock-extra-managed-props syntax-table)))
  (when (not sph-sc-highlight-brackets)
    (setq-local sph-sc-font-lock-keywords-1
      (remq '("[()]" 0 'sph-sc-bracket-face prepend) sph-sc-font-lock-keywords-1))
    (setq-local sph-sc-font-lock-keywords-2
      (remq '("[()]" 0 'sph-sc-bracket-face prepend) sph-sc-font-lock-keywords-2))
    (setq-local sph-sc-font-lock-keywords sph-sc-font-lock-keywords-1))
  (font-lock-refresh-defaults)
  (when (fboundp 'font-lock-flush) (font-lock-flush))
  (when (fboundp 'font-lock-ensure) (font-lock-ensure)))

(provide 'sph-sc-mode)
