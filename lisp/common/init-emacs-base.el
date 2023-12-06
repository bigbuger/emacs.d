;;; init-emacs-base.el --- emacs 自身基本配置
;;; Commentary:
;; 

;;; Code:

;; 处理 symlinks，不然有一些包在访问 symlinks 时有问题，例如 org mode 的 latex 预览
(setq find-file-visit-truename t)

;; emacs 文件备份系统配置
(setq backup-directory-alist '(("." . "~/.cache/emacs/data/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; 记录文件上次打开的位置
(require 'saveplace)
(add-hook 'after-init-hook
	  (save-place-mode))

(desktop-save-mode 1)


(setq Man-notify-method 'pushy)

;; 关闭启动画面
(setq inhibit-startup-message t)

;; 关闭蜂鸣
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; alway hight light
(global-font-lock-mode 1)

;; 用 y 来回答，不用打全 yes
(defalias 'yes-or-no-p 'y-or-n-p)

;; 不要用 gui 的 弹窗提示
(setq use-dialog-box nil)


;; 保存命令历史
(savehist-mode 1)

(auto-image-file-mode)

;; 临时 buffer 根据后缀选择 major-mode
(setq-default major-mode
	      (lambda () (if buffer-file-name
			     (fundamental-mode)
			   (let ((buffer-file-name (buffer-name)))
			     (set-auto-mode)))))

;;(global-linum-mode 1)
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode +1)))
(column-number-mode 1)

;; 长行不要切成两个显示
;; (set-default 'truncate-lines t)

;; 选中文字后，粘贴或者输入自动覆盖选中内容，默认是不开，选中后输入还是追加进去，坑 >_<
(delete-selection-mode 1)

;;像素滚动
(pixel-scroll-precision-mode 1)

(setq mouse-wheel-tilt-scroll t)

;; disable bookmark fringe
(setq bookmark-set-fringe-mark nil)

;; whitch function mode
(which-function-mode)

(add-hook 'prog-mode-hook
	  (lambda ()
	    (setq header-line-format
		  '((which-func-mode ("" which-func-format " "))))))
(setq mode-line-misc-info
      ;; We remove Which Function Mode from the mode line, because it's mostly
      ;; invisible here anyway.
      (assq-delete-all 'which-function-mode mode-line-misc-info))

(add-hook 'prog-mode-hook
	  (lambda ()
	    (font-lock-add-keywords nil
				    '(("\\<\\(FIXME\\|fixme\\|TODO\\|todo\\|BUG\\|bug\\)\\>" 1 font-lock-warning-face t)))))

;; enable narrow
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; make it split horizontal
(setq split-width-threshold 1000)

;; base key ========================================
;;关闭当前缓冲区 Alt+4  ;; C-x 0
(global-set-key (kbd "M-4") 'delete-window)
;;关闭其它缓冲区 Alt+1  ;; C-x 1
(global-set-key (kbd "M-1") 'delete-other-windows)
;;水平分割缓冲区 Alt+2  ;; C-x 2
(global-set-key (kbd "M-2") 'split-window-vertically)
;;垂直分割缓冲区 Alt+3  ;; C-x 3
(global-set-key (kbd "M-3") 'split-window-horizontally)

;;全屏函数
(defun toggle-fullscreen (&optional f)
  "Fullscreen."
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))
(global-set-key [f11] 'toggle-fullscreen)

(provide 'init-emacs-base)

;;; init-emacs-base.el ends here
