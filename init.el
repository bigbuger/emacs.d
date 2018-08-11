(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(set-default-coding-systems 'utf-8)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("PATH" "GOPATH" "GEM_HOME" "GEM_PATH"))
)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq shell-file-name "/bin/bash")

(add-hook 'after-init-hook
	  (lambda ()
	    (load "my-package.el")))

(load "global.el")
(load "my-command.el")
(load "my-hideShow.el")
(load "my-yasnippet.el")
(load "my-go.el")
(load "my-haskell.el")
(load "my-scheme.el")
(load "my-ruby.el")


;;(global-flycheck-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (swiper realgud-byebug realgud-pry realgud company-ghci company-ghc company-quickhelp company-go company all-the-icons neotree counsel ivy magit projectile rvm dash epl flymake-easy go-mode gotest haskell-mode json-reformat json-snatcher pkg-info popup prop-menu flycheck-rebar3 flymake-cursor json-mode idris-mode flymake-haskell-multi flycheck yasnippet visual-regexp undo-tree smex robe exec-path-from-shell inf-ruby ruby-electric rsense go-scratch go-playground go-eldoc go-dlv ghc flymake-ruby color-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white")))))




