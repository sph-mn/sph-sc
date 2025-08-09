(add-to-list (quote load-path) "~/.emacs.d/lisp/lib/mode")
(require (quote sph-sc-mode))

(defun sc-format ()
  "call the shell command \"sc-format\" for the whole buffer or the selected region"
  (interactive)
  (let ((p (point)))
    (call-process-region (point-min) (point-max) "sc-format" t t)
    (goto-char p)))

(add-hook 'sph-sc-mode-hook (lambda () (local-set-key (kbd "M-k") #'sc-format)))
(setq auto-mode-alist (append (quote (("\\.sc\\'" . sph-sc-mode))) auto-mode-alist))
