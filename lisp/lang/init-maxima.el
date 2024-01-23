;;; init-maxima.el --- maxima setup

;;; Commentary:
;; 

(use-package maxima
  :init
  (add-to-list 'auto-mode-alist
	       (cons "\\.mac\\'" 'maxima-mode))
  (add-to-list 'interpreter-mode-alist
	       (cons "maxima" 'maxima-mode)))




(use-package company-maxima
  :after maxima
  :hook
  (maxima-mode . (lambda ()
		   (setq-local company-backends
			       (cl-adjoin '(company-maxima-symbols company-maxima-libraries :with company-yasnippet)
					  company-backends :test #'equal)))))
  
(provide 'init-maxima)

;;; init-maxima.el ends here
