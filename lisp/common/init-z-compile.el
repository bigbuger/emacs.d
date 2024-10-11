;;; init-compile.el --- 编译相关配置
;;; Commentary:
;; 

;;; Code:

(global-set-key (kbd "C-c C-c") #'compile)

;; smart-compile 根据不同文件类型指定不同的编译命令

(require 'smart-compile)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(define-key menu-bar-tools-menu [compile] '("Compile..." . smart-compile))
(define-key prog-mode-map (kbd "s-r") 'smart-compile)
(with-eval-after-load 'projectile
  (define-key projectile-command-map (kbd "r") 'projectile-run-project))

;; end smart-compile

;; realgud
;; (require 'realgud)
;; (load-library "realgud")

;; end realgud

;; rmsbolt 类似于反汇编，联动查看汇编指令/源码
(require 'rmsbolt)

(with-eval-after-load 'rmsbolt
  (add-to-list 'display-buffer-alist
	       `(,rmsbolt-output-buffer
		 display-buffer-in-direction
		 (direction . right)
		 (window-width . 0.5))))
;; end rmsbolt

(provide 'init-z-compile)

;;; init-z-compile.el ends here
