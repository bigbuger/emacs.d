;;; my-latex.el ---

;;; Commentary:
;; 

;;; Code:


(use-package tex
	:ensure auctex
	;; 若使用 straight, 注释前一行, 并取消下一行注释:
	;; :straight auctex
	:custom
	(TeX-parse-self t) ; 自动解析 tex 文件
	(TeX-PDF-mode t)
	(TeX-DVI-via-PDFTeX t)
	:config
	(setq-default TeX-master t) ; 默认询问主文件
	(add-hook 'LaTeX-mode-hook 'turn-on-reftex))

(use-package cdlatex
  :after tex ; 保证 cdlatex 在 auctex 之后加载
  :ensure t
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex))




(require 'xenops)


(provide 'my-latex)


;;; my-latex.el ends here
