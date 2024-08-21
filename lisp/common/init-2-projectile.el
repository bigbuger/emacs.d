;;; init-projectile.el --- project manage setting


;;; Commentary:
;; 

;;; Code:
(require 'projectile)

(projectile-mode)
(setq projectile-require-project-root t)
(setq projectile-indexing-method 'alien)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; (setq projectile-enable-caching t)
(setq projectile-git-submodule-command nil)
(setq projectile-per-project-compilation-buffer t)

(setq frame-title-format
      '(""
	(:eval
	 (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format " [%s] - " project-name))))
	(:eval
	 (let ((filename (buffer-file-name)))
	   (if filename
	       (abbreviate-file-name filename)
	     (buffer-name))))))

(with-eval-after-load 'consult
  (setq consult-project-function 'projectile-project-root))


;; ibuffer
(require 'ibuffer-projectile)
(defun ibuffer-projectile-filter ()
  "Set up `ibuffer-projectile'."
  (ibuffer-projectile-set-filter-groups)
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic)))

(add-hook 'ibuffer-hook #'ibuffer-projectile-filter)

(with-eval-after-load 'magit
  (defun run-projectile-invalidate-cache (&rest _args)
    ;; We ignore the args to `magit-checkout'.
    (projectile-invalidate-cache nil))
  (advice-add 'magit-checkout
              :after #'run-projectile-invalidate-cache)
  (advice-add 'magit-branch-and-checkout ; This is `b c'.
              :after #'run-projectile-invalidate-cache)
  (advice-add 'magit-branch-or-checkout
              :after #'run-projectile-invalidate-cache)
  (advice-add 'magit-pull
	      :after #'run-projectile-invalidate-cache))

(provide 'init-2-projectile)

;;; init-2-projectile.el ends here
