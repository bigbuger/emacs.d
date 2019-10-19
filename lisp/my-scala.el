;;; my-scala.el --- scala config

;;; Commentary:
;; 

;;; Code:

(require 'ensime)
(require 'scala-mode)
(require 'sbt-mode)


(setq ensime-startup-notification nil)
(setq ensime-search-interface 'ivy)

(push '(ensime-company :with company-yasnippet) company-backends)

(provide 'my-scala)

;;; my-scala.el ends here
