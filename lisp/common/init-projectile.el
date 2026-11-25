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

;; 每个项目一个 tab bar。 但是 没法多个项目分屏 :(，先关了
;; (setq tab-bar-show 1) ;; only show tab bar when more then 1

;; ;; copy from https://www.rousette.org.uk/archives/using-the-tab-bar-in-emacs/
;; (defun my-name-tab-by-project-or-default ()
;;   "Return project name if in a project, or default tab-bar name if not.
;; The default tab-bar name uses the buffer name."
;;   (let ((project-name (projectile-project-name)))
;;     (if (string= "-" project-name)
;;         "*Non project*"
;;       (projectile-project-name))))

;; (setq tab-bar-tab-name-function #'my-name-tab-by-project-or-default)

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
	 (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format " [%s] - " project-name))))
	(:eval
	 (let ((filename (buffer-file-name))
	       (vc-name (when vc-mode (cadr (split-string (string-trim vc-mode) "^[A-Z]+[-:]+")))))
	   (cond
	    (vc-name (format "@ %s" vc-name))
	    (filename (abbreviate-file-name filename))
	    (t (buffer-name)))))))

(with-eval-after-load 'consult
  (setq consult-project-function 'projectile-project-root))

;; 每个项目独立进行 xref 跳转
(defvar xref-project-history-hash (make-hash-table :test 'equal)
  "Storage for per-project xref histories.")

(with-eval-after-load 'xref
  (defun xref-project-history-current-project ()
    "Get the current or associated project."
    (or (projectile-project-root)
	""))

  (defun xref--project-history (proj)
    "Return the xref history stored for PROJ, creating it if needed."
    (or (gethash proj xref-project-history-hash)
	(puthash proj (xref--make-xref-history) xref-project-history-hash)))

  (defun xref-project-history (&optional new-value)
    "Return or set a project specific xref-history."
    (let ((proj (xref-project-history-current-project)))
      (if new-value
          (puthash proj new-value xref-project-history-hash)
	(xref--project-history proj))))
  (setq xref-history-storage #'xref-project-history)

  ;; 跨项目跳转带上原来的历史
  (defvar xref--inherit-source-history nil
    "Xref history of the buffer the current jump originates from.")

  (defun xref-set-up-project-history (orig &rest args)
    "Remember the source buffer's xref history."
    (let ((xref--inherit-source-history (xref--get-history)))
      (apply orig args)))

  (advice-add 'xref-pop-to-location :around #'xref-set-up-project-history)
  (advice-add 'xref-goto-xref :around #'xref-set-up-project-history)

  (defun xref-inherit-jump-history ()
    "In the jumped-to buffer, keep using the source project's history."
    (when xref--inherit-source-history
      (let ((src xref--inherit-source-history)
            (proj (xref-project-history-current-project)))
	(unless (eq (xref--project-history proj) src)
          (puthash proj src xref-project-history-hash)))))
  (add-hook 'xref-after-jump-hook #'xref-inherit-jump-history))

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
