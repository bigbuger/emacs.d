;================================================================
;yasnippet
;(add-to-list 'load-path "~/.emacs.d/yasnippet-0.6.1c")
;(require 'yasnippet-bundle)
(add-hook 'c-mode-common-hook 
	  '(lambda()
	     (c-set-style "k&r")))
;(yas/initialize)
(yas-global-mode 1)
(setq yas/root-directory "~/.emacs.d/snippets")
;(yas/load-directory yas/root-directory)

;(global-set-key (kbd "C-i") 'yas/expand)
