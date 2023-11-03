;;; init-scheme.el --- scheme config
;; 
;;; Commentary:
;; 
;;; Code:

(require 'geiser)
(require 'geiser-guile)
(require 'flycheck-guile)

(setq geiser-active-implementations '(guile))
(setq geiser-mode-start-repl-p t)

(require 'smartparens)
;; (sp-local-pair 'scheme-mode "(" nil :actions '(:rem skip))
(define-key scheme-mode-map  (kbd "C-}") 'sp-forward-slurp-sexp)



(provide 'init-scheme)

;;; init-scheme.el ends here
