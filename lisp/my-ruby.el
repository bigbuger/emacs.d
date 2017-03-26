;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ruby                                                      ;;
;(add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(eval-after-load 'auto-complete
      '(add-to-list 'ac-modes 'inf-ruby-mode))
    (add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)
(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "<f5>") 'load-ruby-file)))

(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)

;;;;;;;;;;; end ruby ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
