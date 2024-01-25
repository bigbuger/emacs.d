;;; init-latex.el ---

;;; Commentary:
;; 

;;; Code:


(use-package tex
  :ensure auctex
  :custom
  (TeX-parse-self t) ; 自动解析 tex 文件
  (TeX-PDF-mode t)
  (TeX-DVI-via-PDFTeX t)
  (TeX-source-correlate-mode t) ;; 编译后开启正反向搜索
  (TeX-source-correlate-method 'synctex) ;; 正反向搜索的执行方式
  (TeX-source-correlate-start-server t) ;; 不再询问是否开启服务器以执行反向搜索
  
  :config
  (eval-after-load "preview"
    '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t)) ;; 预览 tikz
  (setq-default TeX-master t) ;; 默认询问主文件
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

(use-package cdlatex
  :demand t
  :ensure t
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
  (add-hook 'org-mode-hook   'turn-on-org-cdlatex))

;; pdf-tools
(use-package pdf-tools
  :ensure t
  :config
  (pdf-loader-install)
  
  ;; Use pdf-tools to open PDF files
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-source-correlate-start-server t)
  
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))


(provide 'init-latex)


;;; init-latex.el ends here
