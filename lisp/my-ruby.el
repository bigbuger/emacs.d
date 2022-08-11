;;; my-ruby.el --- ruby config

;;; Commentary:
;; 

;;; Code:

;;(require 'flymake-ruby)
(require 'rvm)
(require 'inf-ruby)
(require 'robe)

;; Calling dap-ruby-setup
;; gem install ruby-debug-ide -v 0.6.0 or higher versions
;; gem install debase -v 0.2.1 or higher versions
;;
(require 'dap-ruby)

;;(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;;(add-hook 'ruby-mode-hook 'ruby-electric-mode)

(eval-after-load 'company
  '(push '(company-inf-ruby :with company-yasnippet)
	  company-backends))

;;(add-hook 'ruby-mode-hook 'robe-mode)

(rvm-use-default)

;; gem install pry
(setq inf-ruby-default-implementation "pry")

;;(setq inf-ruby-eval-binding "Pry.toplevel_binding")
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)

;; gem install byebug
(load-library "realgud-byebug")

;; gem install solargraph
(add-hook 'ruby-mode-hook #'lsp-deferred)

(add-hook 'ruby-mode-hook 'yard-mode)

(provide 'my-ruby)

;;; my-ruby.el ends here
