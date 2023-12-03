;;; init-latex.el ---

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
  :demand t
  :ensure t
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
  (add-hook 'org-mode-hook   'turn-on-org-cdlatex))




(require 'xenops)


(provide 'init-latex)


;;; init-latex.el ends here
