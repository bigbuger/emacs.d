;;; init-vterm.el --- vterm setting
;;; Commentary:
;; 

;;; Code:

(require 'vterm)
(global-set-key (kbd "C-c t") 'vterm-other-window)
(add-to-list 'vterm-eval-cmds '("find-file-other-window" find-file-other-window))

(with-eval-after-load 'projectile
  (defun projectile-run-vterm-other-window (&optional arg)
    "Invoke `vterm-other-window' in the project's root.

Switch to the project specific term buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead."
    (interactive "P")
    (let* ((project (projectile-acquire-root))
           (buffer (projectile-generate-process-name "vterm" arg project)))
      (unless (buffer-live-p (get-buffer buffer))
	(unless (require 'vterm nil 'noerror)
          (error "Package 'vterm' is not available"))
	(projectile-with-default-dir project
          (vterm-other-window buffer)))
      (switch-to-buffer buffer)))

  (define-key projectile-command-map (kbd "t") 'projectile-run-vterm-other-window))

(use-package multi-vterm :ensure t)

(provide 'init-y-vterm)

;;; init-y-vterm.el ends here
