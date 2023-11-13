;;; init-json.el --- init json mode config


;;; Commentary:
;; 

;;; Code:
(require 'json-mode)

(add-hook 'json-mode-hook 'tree-sitter-mode)
(add-hook 'json-mode-hook 'ts-fold-indicators-mode)

(add-to-list 'load-path "~/.emacs.d/lisp/libs/counsel-jq-ex/")
(require 'counsel-jq-ex)

(define-key json-mode-map (kbd "C-c C-j") 'counsel-jq-ex)
(define-key js-mode-map (kbd "C-c C-j") 'counsel-jq-ex)

(advice-add 'json-mode-beautify :after
	    #'(lambda (&rest _ignore)
		(let ((deactivate deactivate-mark))
		  (if (region-active-p)
		      (indent-region (region-beginning) (region-end))
		    (indent-region (point-min) (point-max)))
		  (setq deactivate-mark deactivate))))



(provide 'init-json)

;;; init-json.el ends here
