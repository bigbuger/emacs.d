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
(define-key scheme-mode-map (kbd "C-}") 'sp-forward-slurp-sexp)
(define-key scheme-mode-map (kbd "C-M-a") 'sp-beginning-of-sexp)
(define-key scheme-mode-map (kbd "C-M-e") 'sp-end-of-sexp)
(define-key scheme-mode-map (kbd "C-<return>") 'sp-up-sexp-and-new-line)

(with-eval-after-load 'org
  (setq org-babel-default-header-args:scheme
	'((:session)))
  (add-to-list 'org-babel-load-languages
	       '(scheme . t)))

(use-package racket-mode
  :ensure t
  :init
  (add-hook 'racket-mode-hook #'racket-xp-mode)
  (setq racket-memory-limit 512)
  (setq racket-command-timeout 6)
  
  (with-eval-after-load 'smart-compile
    (add-to-list 'smart-compile-alist '(racket-mode . "racket %f")))

  :bind (:map racket-mode-map
	      ("C-}"   . sp-forward-slurp-sexp)
	      ("C-M-a" . sp-beginning-of-sexp)
	      ("C-M-e" . sp-end-of-sexp)))

(use-package ob-racket
  :after org
  :load-path "~/.emacs.d/lisp/libs/emacs-ob-racket"
  :config
  (add-to-list 'org-babel-load-languages
	       '(racket . t)))

(use-package pcmpl-args
  :init
  (defalias 'pcomplete/racket 'pcmpl-args-pcomplete-on-help))


(provide 'init-scheme)

;;; init-scheme.el ends here
