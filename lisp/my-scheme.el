(require 'geiser)

(setq geiser-active-implementations '(chez))
(setq geiser-mode-start-repl-p t)
(add-to-list 'company-backends 'geiser-company)
