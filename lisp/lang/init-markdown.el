;;; init-markdown.el --- markdown editor

;;; Commentary:
;; 

(use-package markdown-mode)

;; npm install -g git+https://gitlab.com/matsievskiysv/math-preview
(use-package math-preview
  :bind
  (:map markdown-mode-map
	("C-c C-p" . math-preview-at-point)))

(provide 'init-markdown)

;;; init-markdown.el ends here
