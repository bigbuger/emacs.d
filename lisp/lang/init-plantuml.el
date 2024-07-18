;;; init-plantuml.el --- plantume setting

;;; Commentary:
;; 

;;; Code:

(use-package plantuml-mode
  :ensure t
  
  :init
  (setq plantuml-executable-path "plantuml")
  (setq plantuml-default-exec-mode 'executable)
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (with-eval-after-load 'org-src
    (add-to-list
     'org-src-lang-modes '("plantuml" . plantuml)))
  (progn
    (add-to-list 'load-path "~/.emacs.d/lisp/libs/company-plantuml")
    (require 'company-plantuml))
  
  :hook (plantuml-mode
	 . (lambda ()
	     (setq-local company-backends
			 (cl-adjoin '(company-plantuml :with company-yasnippet)
				    company-backends
				    :test #'equal)))))

(with-eval-after-load 'flycheck
  (flycheck-define-checker plantuml
    "A checker using plantuml.

See `http://plantuml.com"
    :command ("plantuml" "-syntax")
    :standard-input t
    :error-patterns ((error line-start "ERROR" "\n" line "\n" (message) line-end))
    :modes plantuml-mode)

  (add-to-list 'flycheck-checkers 'plantuml))

(with-eval-after-load 'org
  (add-to-list 'org-babel-load-languages
	       '(plantuml . t)))

(provide 'init-plantuml)

;;; init-plantuml.el ends here
