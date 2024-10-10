;;; init-markdown.el --- markdown editor

;;; Commentary:
;; 

(use-package markdown-mode)

;; npm install -g git+https://gitlab.com/matsievskiysv/math-preview
(use-package math-preview
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
