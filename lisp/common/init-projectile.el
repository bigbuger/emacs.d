;;; init-projectile.el --- project manage setting


;;; Commentary:
;; 

;;; Code:
(require 'projectile)

(projectile-mode)
(setq projectile-require-project-root t)
(setq projectile-indexing-method 'alien)
(setq projectile-completion-system 'ivy)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(setq projectile-enable-caching t)

(require 'counsel-projectile)
(define-key projectile-command-map (kbd "s") 'counsel-projectile-rg)
(define-key projectile-command-map (kbd "b") 'counsel-projectile-switch-to-buffer)
;; (define-key projectile-command-map (kbd "f") 'counsel-projectile-find-file)


(setq frame-title-format
      '(""
	"%b"
	(:eval
	 (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format " in [%s]" project-name))))))


;; ibuffer
(require 'ibuffer-projectile)
(defun ibuffer-projectile-filter ()
  "Set up `ibuffer-projectile'."
  (ibuffer-projectile-set-filter-groups)
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic)))

(add-hook 'ibuffer-hook #'ibuffer-projectile-filter)

(provide 'init-projectile)

;;; init-projectile.el ends here
