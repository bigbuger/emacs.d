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
  (add-to-list 'org-babel-load-languages
	       '(scheme . t)))

(provide 'init-scheme)

;;; init-scheme.el ends here
