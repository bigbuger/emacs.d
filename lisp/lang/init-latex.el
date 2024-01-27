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
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'prettify-symbols-mode))

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

;; xenops 支持 svg 预览
;; 如果 tikz 预览失败，需配置 export LIBGS=/opt/homebrew/Cellar/ghostscript/10.02.1/lib/libgs.dylib
(use-package xenops
  :config
  (setq xenops-reveal-on-entry t)
  (setq xenops-math-image-scale-factor 1.7)
  :hook
  (LaTeX-mode . xenops-mode))

(defun my/text-scale-adjust-latex-previews ()
  "Adjust the size of latex preview fragments when changing the
buffer's text scale."
  (pcase major-mode
    ('latex-mode
     (cond
      (xenops-mode
       (cond ((= 0 text-scale-mode-amount)
	      (progn (xenops-reveal)
		     (xenops-render)))
	     ((> text-scale-mode-amount 0)
	      (dotimes (_ text-scale-mode-amount)
		(xenops-increase-size)))
	     (t (dotimes (_ (abs text-scale-mode-amount))
		(xenops-decrease-size)))))
      (t (dolist (ov (overlays-in (point-min) (point-max)))
	   (if (eq (overlay-get ov 'category)
		   'preview-overlay)
               (my/text-scale--resize-fragment ov))))))
    ('org-mode
     (dolist (ov (overlays-in (point-min) (point-max)))
       (if (eq (overlay-get ov 'org-overlay-type)
               'org-latex-overlay)
           (my/text-scale--resize-fragment ov))))))

(defun my/text-scale--resize-fragment (ov)
  (overlay-put
   ov 'display
   (cons 'image
         (plist-put
          (cdr (overlay-get ov 'display))
          :scale (+ 1.0 (* 0.25 text-scale-mode-amount))))))

(add-hook 'text-scale-mode-hook #'my/text-scale-adjust-latex-previews)

(provide 'init-latex)


;;; init-latex.el ends here
