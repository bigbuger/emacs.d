;;; init-projectile.el --- project manage setting


;;; Commentary:
;; 

;;; Code:
(require 'projectile)
(require 'cl-lib)

(projectile-mode)
(setq projectile-require-project-root t)
(setq projectile-indexing-method 'alien)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; (define-key projectile-command-map (kbd "c") 'projectile-compile-project)
(define-key projectile-command-map (kbd "r") 'projectile-run-project)

(setq tab-bar-show 1) ;; only show tab bar when more then 1

;; copy from https://www.rousette.org.uk/archives/using-the-tab-bar-in-emacs/
(defun my-name-tab-by-project-or-default ()
  "Return project name if in a project, or default tab-bar name if not.
The default tab-bar name uses the buffer name."
  (let ((project-name (projectile-project-name)))
    (if (string= "-" project-name)
        "*Non project*"
      (projectile-project-name))))

(setq tab-bar-tab-name-function #'my-name-tab-by-project-or-default)

;; 每个项目一个 tab bar。 但是 没法多个项目分屏 :(，先关了
;; (projectile-session-mode +1)

(setq projectile-per-project-compilation-buffer t)

;; (setq projectile-enable-caching t)
;; (setq projectile-git-submodule-command nil)

;; make git submodule the same root of parent
;; see https://github.com/bbatsov/projectile/issues/712
;; (setq projectile-project-root-files-bottom-up
;;       (remove ".git" projectile-project-root-files-bottom-up))
;; (add-to-list 'projectile-project-root-files-top-down-recurring ".git")
(cl-nsubstitute ".git/" ".git" projectile-project-root-files-bottom-up :test #'string=)

(defun my-projectile-ignore-git-project (project-root)
    (string-match-p ".git" project-root))
(setq projectile-ignored-project-function #'my-projectile-ignore-git-project)

(setq frame-title-format
      '(""
	(:eval
	 (let ((project-name (projectile-project-name))
	       (filename (buffer-file-name)))
           (cond
	    ((not (string= "-" project-name))
	     (format " [%s]" project-name))
	    (filename (abbreviate-file-name (buffer-file-name)))
	    (t (buffer-name)))))

	(:eval
	 (let ((root (projectile-project-root)))
	   (when (and root (eq 'git (ignore-errors (projectile-project-vcs root))))
	     (format " @ %s"
		     (plist-get (projectile-dashboard--git-status (projectile-project-root)) :branch)))))))

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

(provide 'init-projectile)

;;; init-projectile.el ends here
