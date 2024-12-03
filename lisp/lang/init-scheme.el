;;; init-scheme.el --- scheme config
;; 
;;; Commentary:
;; 
;;; Code:

(require 'geiser)
(require 'geiser-guile)
(require 'flycheck-guile)

(setq geiser-active-implementations '(guile))

(require 'smartparens)
;; (sp-local-pair 'scheme-mode "(" nil :actions '(:rem skip))
(define-key scheme-mode-map  (kbd "C-}") 'sp-forward-slurp-sexp)

(with-eval-after-load 'org
  (setq org-babel-default-header-args:scheme
	'((:session)))
  (add-to-list 'org-babel-load-languages
	       '(scheme . t)))

(use-package racket-mode
  :ensure t
  :init
  (add-hook 'racket-mode-hook #'racket-xp-mode)
  (setq racket-memory-limit 512))

(use-package ob-racket
  :after org
  :load-path "~/.emacs.d/lisp/libs/emacs-ob-racket"
  :config
  (add-to-list 'org-babel-load-languages
	       '(racket . t)))


(provide 'init-scheme)

;;; init-scheme.el ends here
