(require 'package)
;;(add-to-list 'package-archives
;;             '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;                         ("stable-melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")
;;			 ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

(set-default-coding-systems 'utf-8)
(define-coding-system-alias 'UTF-8 'utf-8)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'my-package)

(when (memq window-system '(mac ns))
  (set-fontset-font t nil (font-spec :family "Apple Color Emoji") nil 'append))

(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-envs '("PATH" "GOPATH" "GEM_HOME" "GEM_PATH"))


(require 'global)
(require 'my-command)
(require 'my-org)
(require 'my-go)
(require 'my-haskell)
(require 'my-agda)
(require 'my-scheme)
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
(require 'theme-settig)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'pushy)
 '(centaur-tabs-background-color "white")
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(dap-python-executable "python3")
 '(display-time-mode t)
 '(doom-modeline-mode t)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(flycheck-checker-error-threshold 600)
 '(flycheck-pos-tip-max-width 20)
 '(highlight-parentheses-colors '("LightPink4" "dark cyan" "turquoise3" "IndianRed4"))
 '(lsp-ui-flycheck-enable nil t)
 '(package-selected-packages
   '(lsp-ivy gorepl-mode ob-go highlight-parentheses go-tag go-fill-struct godoctor go-impl go-gen-test multiple-cursors flycheck-posframe pyim dap-mode ebnf-mode dumb-jump sqlformat yard-mode dirvish company-inf-ruby ejc-sql rustic ivy-hydra company-fuzzy impostman verb ob-restclient projectile-ripgrep tree-sitter-langs tree-sitter vterm magic-latex-buffer auctex figlet ripgrep counsel-fd doom-modeline all-the-icons-ivy-rich ztree dante csv-mode gnuplot-mode spacemacs-theme imenu-list olivetti org-bullets crux flycheck-pos-tip google-this all-the-icons-dired git-timemachine wgrep highlight-indent-guides diff-hl spotlight aggressive-indent expand-region flycheck-rust go-guru auto-yasnippet move-text treemacs treemacs-projectile company-shell buffer-move smartparens string-inflection ace-window company-ansible docker-compose-mode dockerfile-mode ivy-rich company-restclient go-eldo))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:foreground "red" :height 5.0))))
 '(diff-hl-change ((t (:background "blue3" :foreground "blue3"))))
 '(diff-hl-delete ((t (:inherit diff-removed :background "red3" :foreground "red3"))))
 '(diff-hl-insert ((t (:inherit diff-added :background "green4" :foreground "green4"))))
 '(flycheck-error ((t (:underline "Red1"))))
 '(flycheck-warning ((t (:underline (:color "yellow4" :style wave)))))
 '(flymake-warning ((t (:underline (:color "yellow4" :style wave)))))
 '(ivy-posframe ((t (:inherit default :background "gray40" :foreground "#dcdccc"))))
 '(show-paren-match ((t (:inherit bold :foreground "royal blue" :underline t))))
 '(term-color-blue ((t (:background "SkyBlue3" :foreground "SkyBlue3")))))




(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
