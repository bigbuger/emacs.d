;;; init-latex.el ---

;;; Commentary:
;; 

;;; Code:


(use-package tex
  :ensure auctex
  :custom
  (TeX-parse-self t)			; 自动解析 tex 文件
  (TeX-PDF-mode t)
  (TeX-DVI-via-PDFTeX t)
  (TeX-source-correlate-mode t)		 ;; 编译后开启正反向搜索
  (TeX-source-correlate-method 'synctex) ;; 正反向搜索的执行方式
  (TeX-source-correlate-start-server t) ;; 不再询问是否开启服务器以执行反向搜索
  
  :config
  (setq-default TeX-master t) ;; 默认询问主文件
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

  (with-eval-after-load 'tex-mode
    (assoc-delete-all "--" tex--prettify-symbols-alist)
    (assoc-delete-all "---" tex--prettify-symbols-alist))
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (add-hook 'LaTeX-mode-hook #'prettify-symbols-mode))

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

(with-eval-after-load 'org
  (add-to-list 'org-babel-load-languages
	       '(latex . t)))

(when (file-exists-p "/usr/local/share/asymptote/asy-mode.el")
  (add-to-list 'load-path "/usr/local/share/asymptote")
  (autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
  (autoload 'lasy-mode "asy-mode.el" "hybrid Asymptote/Latex major mode." t)
  (autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
  (add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))

  (require 'lsp-mode)
  (add-to-list 'lsp-language-id-configuration '(asy-mode . "asymptote"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("asy" "-lsp"))
                    :activation-fn (lsp-activate-on "asymptote")
                    :major-modes '(asy-mode)
                    :server-id 'asyls))
  
  ;; (add-hook 'asy-mode-hook #'lsp-deferred)
  )

(use-package ob-asymptote
  :init
  (with-eval-after-load 'org
    (add-to-list 'org-babel-load-languages
		 '(asymptote . t))))

(defconst ob-metapost-output-buffer-name "*Org babel metapost*")

(defun org-babel-execute:metapost (body params)
  "Execute a block of metapost code with org-babel."
  (let* ((in-file (org-babel-temp-file "metapost" ".mp"))
	 (cmdline (or (cdr (assq :cmdline params)) ""))
	 (result-type (cdr (assq :results params)))
	 (out-file (cdr (assq :file params)))
	 (out-format (when out-file (file-name-extension out-file)))
	 (tmp-file (when out-file (file-name-with-extension (file-name-base in-file) out-format)))
	 (out-dir default-directory)
	 (work-dir (file-name-directory in-file)))
    (with-temp-file in-file
      (insert body))
    (message "out-file: %s, out-format: %s" out-file out-format)
    (let* ((default-directory work-dir)
	   (cmd (format "mpost -halt-on-error %s %s %s %s"
			(if out-file
			    (format "-s 'outputtemplate=\"%s\"'" tmp-file)
			  "")
			(if out-format
			    (format "-s 'outputformat=\"%s\"'" out-format)
			  "")
			cmdline (org-babel-process-file-name in-file))))
      (shell-command cmd ob-metapost-output-buffer-name ob-metapost-output-buffer-name)
      (when out-file
	(copy-file tmp-file (concat out-dir "/" out-file) t))
      (with-current-buffer ob-metapost-output-buffer-name
	(if (member "output" (split-string result-type))
	    (buffer-string)
	nil)))))

(with-eval-after-load 'org
  (add-to-list 'org-babel-load-languages
	       '(metapost . t)))

(provide 'ob-metapost)

(with-eval-after-load 'smart-compile
  (setf (alist-get "\\.mp\\'" smart-compile-alist nil nil #'string=) "mpost %f"))

(use-package pcmpl-args
  :init
  (defalias 'pcomplete/mpost 'pcmpl-args-pcomplete-on-help))

(provide 'init-latex)


;;; init-latex.el ends here
