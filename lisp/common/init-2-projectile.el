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
(define-key projectile-command-map (kbd "c") 'projectile-compile-project)
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

;; 每个项目独立进行 xref 跳转
(defvar xref-project-history-hash (make-hash-table :test 'equal)
  "Storage for per-project xref histories.")

(defun xref-project-history-current-project ()
  "Get the current or associated project."
  (or (projectile-project-root)
      ""))

(defun xref-project-history (&optional new-value)
  "Return or set a project specific xref-history."
  (let ((proj (xref-project-history-current-project)))
    (if new-value
        (puthash proj new-value xref-project-history-hash)
      (or
       (gethash proj xref-project-history-hash)
       (puthash proj (xref--make-xref-history) xref-project-history-hash)))))
(setq xref-history-storage #'xref-project-history)

;; projectile rg 有时候比 consult-rg 好用
(define-key projectile-mode-map (kbd "C-c p s") 'projectile-ripgrep)

(define-key projectile-command-map (kbd "t") 'projectile-run-vterm-other-window)

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
