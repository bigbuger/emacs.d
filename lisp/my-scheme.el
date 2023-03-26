;;; my-scheme.el --- scheme config
;; 
;;; Commentary:
;; 
;;; Code:

(require 'geiser)
(require 'geiser-guile)
(require 'flycheck-guile)

(setq geiser-active-implementations '(guile))
(setq geiser-mode-start-repl-p t)
(push '(geiser-company :with company-yasnippet) company-backends)


(require 'smartparens)
(sp-local-pair 'scheme-mode "(" nil :actions '(:rem insert skip))
(define-key scheme-mode-map  (kbd "C-}") 'sp-forward-slurp-sexp)



(provide 'my-scheme)

;;; my-scheme.el ends here
