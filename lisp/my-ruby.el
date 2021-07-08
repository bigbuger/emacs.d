;;; my-ruby.el --- ruby config

;;; Commentary:
;; 

;;; Code:

;;(require 'flymake-ruby)
(require 'rvm)
(require 'inf-ruby)
(require 'robe)

;;(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;;(add-hook 'ruby-mode-hook 'ruby-electric-mode)

(eval-after-load 'company
  '(push '(company-inf-ruby company-robe :with company-yasnippet)
	  company-backends))

(add-hook 'ruby-mode-hook 'robe-mode)

(rvm-use-default)

(setq inf-ruby-default-implementation "pry")
;;(setq inf-ruby-eval-binding "Pry.toplevel_binding")
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)

(load-library "realgud-byebug")

;;;;;;;;;;; end ruby ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'my-ruby)

;;; my-ruby.el ends here
