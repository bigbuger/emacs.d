(require 'ensime)
(require 'scala-mode)
(require 'sbt-mode)

(setq ensime-startup-notification nil)
(setq ensime-search-interface 'ivy)

(push '(ensime-company :with company-yasnippet) company-backends)
