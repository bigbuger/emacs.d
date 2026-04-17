;;; init-markdown.el --- markdown editor

;;; Commentary:
;; 

(use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-enable-math t))

(use-package md-mermaid
  :vc (:url "https://github.com/ahmetus/md-mermaid" :rev :newest)
  :commands (md-mermaid-render-current md-mermaid-transient))

(modify-coding-system-alist 'file "\\.md\\'" 'utf-8)

;; npm install -g git+https://gitlab.com/matsievskiysv/math-preview
(use-package math-preview
  :config
  (setq math-preview-scale 1.3)
  
  :bind
  (:map markdown-mode-map
	("C-c C-p" . math-preview-at-point)))


(use-package company-math
  :init
  (add-hook 'markdown-mode-hook
            (lambda ()
	      (setq-local company-math-allow-latex-symbols-in-faces t)
	      (setq-local company-math-allow-unicode-symbols-in-faces t)
	      (setq-local company-backends
			  (cl-adjoin '(company-math-symbols-latex :with company-yasnippet) company-backends :test #'equal)))))

(provide 'init-markdown)

;;; init-markdown.el ends here
