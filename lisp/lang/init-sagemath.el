;;; init-sagemath.el --- sage math setting

;;; Commentary:
;; 

;;; Code:

(use-package sage-shell-mode
  :init
  ;; Run SageMath by M-x run-sage instead of M-x sage-shell:run-sage
  (sage-shell:define-alias)

;; Turn on eldoc-mode in Sage terminal and in Sage source files
  (add-hook 'sage-shell-mode-hook #'eldoc-mode)
  (add-hook 'sage-shell:sage-mode-hook #'eldoc-mode))

(use-package ob-sagemath
  :init
  ;; Ob-sagemath supports only evaluating with a session.
  (setq org-babel-default-header-args:sage '((:session . t)
                                             (:results . "output")))
  (with-eval-after-load 'org
    (add-to-list 'org-babel-load-languages
		 '(sagemath . t))))

(use-package company-sage
  :load-path "~/.emacs.d/lisp/libs/company-sage"
  :hook (sage-shell:sage-mode
	 . (lambda ()
	     (setq-local company-backends
			 (cl-adjoin '(company-sage :with company-yasnippet)
				    company-backends
				    :test #'equal)))))


(provide 'init-sagemath)

;;; init-sagemath.el ends here
