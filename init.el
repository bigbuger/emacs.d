(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(set-default-coding-systems 'utf-8)


(setq shell-file-name "/bin/bash")
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)

(when (memq window-system '(mac ns))
  (let (
  	(mypaths
  	 (append (split-string (getenv "PATH") ":")
  		 (list
  		  "/usr/local/bin"
  		  "/usr/local/go/bin"
  		  (concat (getenv "HOME") "/Library/Haskell/bin")
  		  (concat (getenv "HOME") "/go/bin")))))
    (setenv "PATH"  (mapconcat 'identity mypaths ":"))
    (setenv "GOPATH" (concat (getenv "HOME") "/go"))
    (setq exec-path (append mypaths (list "." exec-directory)) ))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("GEM_HOME" "GEM_PATH"))
)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(load "global.el")
(load "my-command.el")
(load "my-hideShow.el")
(load "my-autoComple.el")
(load "my-yasnippet.el")
(load "my-go.el")
(load "my-haskell.el")
(load "my-erlang.el")
(load "my-scheme.el")
(load "my-ruby.el")
(load "my-package.el")



;;(global-flycheck-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (all-the-icons neotree counsel ivy magit projectile rvm auto-complete dash distel-completion-lib epl flymake-easy go-mode gotest haskell-mode json-reformat json-snatcher pkg-info popup prop-menu flycheck-rebar3 auto-complete-distel erlang flymake-cursor json-mode idris-mode flymake-haskell-multi flycheck yasnippet visual-regexp undo-tree smex robe exec-path-from-shell inf-ruby ac-inf-ruby ruby-electric rsense go-scratch go-playground go-eldoc go-dlv go-autocomplete ghc flymake-ruby color-theme ac-haskell-process))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )




