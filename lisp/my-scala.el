;;; my-scala.el --- scala config

;;; Commentary:
;; 

;;; Code:

(require 'ensime)
(require 'scala-mode)
(require 'sbt-mode)


(setq ensime-startup-notification nil)
(setq ensime-search-interface 'ivy)


(add-hook 'protobuf-mode-hook
          (lambda () (setq-local company-backends
				 (cl-adjoin '(ensime-company :with company-yasnippet) company-backends :test #'equal))))

(provide 'my-scala)

;;; my-scala.el ends here
