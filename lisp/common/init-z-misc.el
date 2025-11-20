;;; init-misc.el --- 不知道怎么分类的配置🤣



;;; Commentary:
;; 

;;; Code:

;; gpg 配置
;; Put this in ~/.gnupg/gpg-agent.conf:
;; allow-emacs-pinentry
;; allow-loopback-pinentry
(setq epa-pinentry-mode 'loopback)

;; dumb-jump 跳转到代码定义
(require 'dumb-jump)
(setq dumb-jump-force-searcher 'rg)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(with-eval-after-load 'c-mode
  (unbind-key "C-c ." 'c-mode-base-map))

(with-eval-after-load 'protobuf-mode
  (unbind-key "C-c ." 'protobuf-mode-map))

(global-set-key (kbd "C-c .") 'dumb-jump-go)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

;; which-key 快捷键打小抄
(require 'which-key)
(which-key-mode)

(setopt which-key-allow-multiple-replacements t
        which-key-sort-order 'which-key-description-order ;; 不要重新排序，按照按键绑定的先后顺序就行
        which-key-replacement-alist
          (seq-map
           (lambda (rep)
             `((nil . ,(elt rep 0))
               . (nil . ,(elt rep 1))))
           '(("org-babel-" "ob-")
             ("string-inflection-" "")
	     ("embark-" "")
	     ("lsp-ui-" "")
	     ("lsp-" "")
	     ("xref-" "")
             ("embark-collect" "⇶ collect")
             ("embark-export" "⇶ export")
             ("embark-act-all" "all")
             ("embark-become" "become")
             ("-" " "))))

;; imenu-list 侧边栏显示 imenu
(require 'imenu-list)
(global-set-key (kbd "<f9>") 'imenu-list)
(setq imenu-auto-rescan t)

;; helpful 更好的帮助文档
(require 'helpful)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)
(global-set-key (kbd "C-h f") #'helpful-function)
(global-set-key (kbd "C-h v") #'helpful-variable)

(define-key emacs-lisp-mode-map (kbd "C-c C-d") #'helpful-at-point)

;; ace-window 快速通过数字切换到指定窗口
(require 'ace-window)
(global-set-key (kbd "C-c o") 'ace-window)

;; nerd-icons-ibuffer
(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))


;; vlf 打开文件
(use-package vlf
  :init
  (require 'vlf-setup))


;; 项目模版系统
(use-package skeletor
  :ensure t
  :custom
  (skeletor-completing-read-function #'completing-read-default) ;; use default, aka: ivy or vertical or what every activate.
  :bind (("s-n" . skeletor-create-project-at)))


;; (use-package dimmer
;;   :init
;;   (dimmer-configure-which-key)
;;   (dimmer-mode t))

(add-to-list 'load-path "~/.emacs.d/lisp/libs")
(require 'point-undo)
(global-set-key (kbd "s-[") #'point-undo)
(global-set-key (kbd "s-]") #'point-redo)

(use-package pcre2el)

;; pcmpl-args extends option and argument completion of shell commands read by Emacs.
;; It is intended to make shell completion in Emacs comparable to the rather excellent completion provided by both Bash and Zsh.
(use-package pcmpl-args
  :init
  (defalias 'pcomplete/rg 'pcmpl-args-pcomplete-on-help))

(setq Info-additional-directory-list '("/opt/homebrew/share/info"))

(use-package proced
  :ensure nil
  :commands proced
  :custom
  (proced-auto-update-flag t)
  (proced-goal-attribute nil)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t)
  (proced-format 'custom)
  :config
  (add-to-list
   'proced-format-alist
   '(custom user pid ppid sess tree pcpu pmem rss start time state (args comm))))

(provide 'init-z-misc)

;;; init-z-misc.el ends here
