(require 'geiser)

(setq geiser-active-implementations '(chez))
(setq geiser-mode-start-repl-p t)
(push '(geiser-company :with company-yasnippet) company-backends)
