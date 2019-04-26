(require 'package)
(add-to-list 'package-archives
             '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(set-default-coding-systems 'utf-8)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "my-package.el")

(when (memq window-system '(mac ns))
  (set-fontset-font t nil (font-spec :family "Apple Color Emoji") nil 'append))

(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-envs '("PATH" "GOPATH" "GEM_HOME" "GEM_PATH"))

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
  )


(load "global.el")
(load "my-command.el")
(load "my-go.el")
(load "my-haskell.el")
(load "my-scheme.el")
(load "my-ruby.el")
(load "my-python.el")
(load "my-scala")

;============================================================
;全屏函数
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))
(global-set-key [f11] 'toggle-fullscreen)

;;============================================================
;; 颜色主题
(set-face-attribute 'default nil :height 140)

;;(require 'color-theme)
;;(color-theme-initialize)
(add-to-list 'load-path "~/.emacs.d/lisp/tomorrow-theme")
(require 'tomorrow-night-eighties-theme)
;;(setq sml/theme 'respectful)
(setq sml/no-confirm-load-theme t)
(sml/setup)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-time-mode t)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(package-selected-packages
   (quote
    (treemacs treemacs-magit treemacs-projectile company-shell graphviz-dot-mode buffer-move smartparens which-key string-inflection ace-window company-ansible docker-compose-mode dockerfile-mode docker ivy-rich smart-mode-line company-restclient restclient magit-gitflow go-eldo)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:foreground "red" :height 5.0))))
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white"))))
 '(flycheck-warning ((t (:underline (:color "yellow2" :style wave)))))
 '(flymake-warning ((t (:underline (:color "yellow2" :style wave)))))
 '(ivy-posframe ((t (:inherit default :background "gray40" :foreground "#dcdccc"))))
 '(term-color-blue ((t (:background "SkyBlue3" :foreground "SkyBlue3")))))




