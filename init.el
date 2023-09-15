(require 'package)
;;(add-to-list 'package-archives
;;             '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

 (setq package-archives '(("gnu"   . "http://1.15.88.122/gnu/")
                          ("melpa" . "http://1.15.88.122/melpa/")
			  ("org"   . "http://1.15.88.122/org/")))

(package-initialize)

(set-default-coding-systems 'utf-8)
(define-coding-system-alias 'UTF-8 'utf-8)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'my-package)

(when (memq window-system '(mac ns))
  (set-fontset-font t nil (font-spec :family "Apple Color Emoji") nil 'append))

(require 'exec-path-from-shell)
(setq exec-path-from-shell-variables
      (append exec-path-from-shell-variables '("LC_ALL" "LANG" "GOPATH" "GEM_HOME" "GEM_PATH" "JAVA_HOME")))
(exec-path-from-shell-initialize)


(require 'global)
(require 'my-command)
(require 'my-org)
(require 'my-go)
(require 'my-haskell)
(require 'my-agda)
(require 'my-scheme)
(require 'my-clojure)
(require 'my-ruby)
(require 'my-python)
(require 'my-rust)
(require 'my-cc)
(require 'my-octave)
(require 'my-json)
(require 'my-web)
(require 'my-ejc)
(require 'my-latex)
(require 'my-liquid)
(require 'my-protobuf)
(require 'my-elm)
(require 'my-scala)
(require 'theme-settig)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(package-selected-packages
   '(company-ghci yatemplate realgud-lldb idris-mode clj-refactor color-theme-sanityinc-tomorrow company-fuzzy json-mode rainbow-mode google-translate rmsbolt counsel-dash helpful elm-mode smart-compile xenops magit-delta regex-tool all-the-icons-ibuffer ivy-emoji centaur-tabs ibuffer-projectile bufler auto-highlight-symbol flyspell-correct-popup eldoc-box go-playground osx-trash protobuf-mode gorepl-mode ob-go go-tag go-fill-struct godoctor go-impl go-gen-test pyim dap-mode ebnf-mode dumb-jump sqlformat yard-mode company-inf-ruby ejc-sql rustic ivy-hydra impostman verb ob-restclient projectile-ripgrep tree-sitter-langs vterm magic-latex-buffer auctex counsel-fd doom-modeline all-the-icons-ivy-rich ztree dante csv-mode gnuplot-mode spacemacs-theme imenu-list org-bullets crux all-the-icons-dired git-timemachine wgrep highlight-indent-guides diff-hl spotlight expand-region flycheck-rust auto-yasnippet move-text treemacs-projectile company-shell buffer-move smartparens string-inflection ace-window company-ansible docker-compose-mode dockerfile-mode ivy-rich company-restclient go-eldo))
 '(pdf-tools-handle-upgrades t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
