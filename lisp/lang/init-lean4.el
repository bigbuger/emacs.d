;;; init-lean4.el --- lean4


;;; Commentary:
;; 

;;; Code:

(use-package lean4-mode
  :load-path "~/.emacs.d/lisp/libs/lilean4-mode"

  :config
  (with-eval-after-load 'company-keywords
    (add-to-list 'company-keywords-alist
		 `(lean4-mode ,@lean4-keywords1)))
  (add-hook 'lean4-mode-hook
	    (lambda ()
	      (setq-local company-backends
			  '((company-capf company-keywords company-yasnippet)
			    company-files
			    company-dabbrev-code
			    company-dabbrev))))
  )

(provide 'init-lean4)

;;; init-lean4.el ends here
