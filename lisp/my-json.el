;;; my-json.el --- my json mode config


;;; Commentary:
;; 

;;; Code:
(require 'json-mode)

(add-to-list 'load-path "~/.emacs.d/lisp/counsel-jq-ex/")
(require 'counsel-jq-ex)

(add-hook 'json-mode-hook 'hideshowvis-enable)
(define-key json-mode-map (kbd "C-c C-j") 'counsel-jq-ex)
(define-key js-mode-map (kbd "C-c C-j") 'counsel-jq-ex)

(advice-add 'json-mode-beautify :after
	    #'(lambda (&rest _ignore)
		(let ((deactivate deactivate-mark))
		  (if (region-active-p)
		      (indent-region (region-beginning) (region-end))
		    (indent-region (point-min) (point-max)))
		  (setq deactivate-mark deactivate))))



(provide 'my-json)

;;; my-json.el ends here
