;;; my-scheme.el --- scheme config
;; 
;;; Commentary:
;; 
;;; Code:

(require 'geiser)

(setq geiser-active-implementations '(chez))
(setq geiser-mode-start-repl-p t)
(push '(geiser-company :with company-yasnippet) company-backends)

(provide 'my-scheme)

;;; my-scheme.el ends here
