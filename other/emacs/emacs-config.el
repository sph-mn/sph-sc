(defun sc-format ()
  "call the shell command \"sc-format\" for the whole buffer or the selected region"
  (interactive)
  (let ((prev-point (point))) (call-process-region (point-min) (point-max) "sc-format" t t)
    (goto-char prev-point)))

; an alternative would be (load-library "full-path")
(add-to-list (quote load-path) "~/.emacs.d/lisp/lib/mode")
(require (quote sph-sc-mode))

(add-hook (quote sph-sc-mode-hook)
  (lambda ()
    (define-key sph-sc-mode-map "\M-k" (quote sc-format))))

; the mode is activated with calling "sph-sc-mode"
(setq auto-mode-alist (append (quote ((".sc$" . sph-sc-mode))) auto-mode-alist))
