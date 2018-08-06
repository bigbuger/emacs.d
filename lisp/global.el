;;关闭启动画面
(setq inhibit-startup-message t)
(setq frame-title-format "emacs@%b")

;;关闭蜂鸣
(setq visible-bell t)
;;alway hight light
(global-font-lock-mode 1)

;;ido
(ido-mode 1)
(setq ido-auto-merge-work-directories-length -1)

;;projectile
(projectile-global-mode)
(setq projectile-require-project-root nil)
(global-set-key (kbd "M-<f2>") 'projectile-speedbar-open-current-buffer-in-tree)

;;括号匹配
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

;;显示时间
(display-time)

(which-function-mode)

(auto-image-file-mode)

;; 自动保存
(require 'auto-save)            ;; 加载自动保存模块
(auto-save-enable)              ;; 开启自动保存功能
(setq auto-save-slient t)       ;; 自动保存的时候静悄悄的， 不要打扰我


(global-linum-mode 1)

;; smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; sr-speebar
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-auto-refresh t)
(setq speedbar-show-unknown-files t)
(global-set-key [f8] 'sr-speedbar-toggle)

;; undo-tree
(global-undo-tree-mode)

;; visual=regexp
(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)

(eval-after-load 'flymake '(require 'flymake-cursor))

(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)

;=========================================
;(global-set-key (kbd "C-c j") 'goto-line)
;(global-set-key (kbd "M-s") 'query-replace-regexp)

;;========================================
;;关闭当前缓冲区 Alt+4  ;; C-x 0
(global-set-key (kbd "M-4") 'delete-window)
;;关闭其它缓冲区 Alt+1  ;; C-x 1
(global-set-key (kbd "M-1") 'delete-other-windows)
;;水平分割缓冲区 Alt+2  ;; C-x 2
(global-set-key (kbd "M-2") 'split-window-vertically)
;;垂直分割缓冲区 Alt+3  ;; C-x 3
(global-set-key (kbd "M-3") 'split-window-horizontally)
;;切换到其它缓冲区 Alt+0 ;; C-x o 
(global-set-key (kbd "M-0") 'other-window)
;;============================================================
;;窗口之间移到
(global-set-key (kbd "C-c M-n") 'windmove-down)
(global-set-key (kbd "C-c M-p") 'windmove-up)
(global-set-key (kbd "C-c M-f") 'windmove-right)
(global-set-key (kbd "C-c M-b") 'windmove-left)

(setq org-src-fontify-natively t)

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
;;flymake基本配置
;;(autoload 'flymake-find-file-hook "flymake" "" t)
;;(add-hook 'find-file-hook 'flymake-find-file-hook)
;;(setq flymake-gui-warnings-enabled nil)	;关闭错误对话框
;;(setq flymake-log-level 0)

;;============================================================

;============================================================
;颜色主题
;(add-to-list 'load-path "~/.emacs.d/color-theme")
(require 'color-theme)
(color-theme-initialize)
;(color-theme-tty-dark)
(add-to-list 'load-path "~/.emacs.d/lisp/tomorrow-theme")
(require 'tomorrow-night-eighties-theme)
