;;; init-calc.el --- emacs calc


;;; Commentary:
;; 

;;; Code:

(use-package vertico-calc
  :demand t
  :load-path "~/.emacs.d/lisp/libs/"
  :bind (("C-c q" . vertico-calc))
  :init
  (with-eval-after-load 'calc
    (setq calc-date-format '(YYYY "-" MM "-" DD (" " hh ":" mm ":" ss))))
  (defun +calc-complete (fun &rest args)
    (minibuffer-with-setup-hook
      (:append
       (lambda ()
	 (use-local-map calc-completion-map)
	 (add-hook 'completion-at-point-functions
		   #'vertico-calc-completion-at-point nil t)))
      (apply fun args)))
  (advice-add 'quick-calc :around #'+calc-complete))

(use-package literate-calc-mode
  :ensure t
  :demand t
  :bind
  (("C-* l" . literate-calc-minor-mode))

  :config
  (add-hook 'literate-calc-mode-hook
	    #'(lambda ()
		(add-hook 'completion-at-point-functions
			  #'vertico-calc-completion-at-point nil t)
		(setq-local company-backends
			    '((company-capf :with company-yasnippet :with company-dabbrev :with company-files))))))

(provide 'init-calc)

;;; init-calc.el ends here
