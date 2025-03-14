;;; init-lean4.el --- lean4


;;; Commentary:
;; 

;;; Code:

(use-package lean4-mode
  :load-path "~/.emacs.d/lisp/libs/lean4-mode"

  :config
  (define-key lean4-mode-map (kbd "s-l s-s") #'lean4-insert-show)
  (setq lean4-info-buffer-debounce-delay-sec 0.2)
  (setq lean4-info-buffer-debounce-upper-bound-sec 0.3)
  
  (add-to-list 'display-buffer-alist
	       `(,lean4-info-buffer-name
		 display-buffer-in-direction
		 (direction . right)))
  
  (with-eval-after-load 'company-keywords
    (add-to-list 'company-keywords-alist
		 `(lean4-mode ,@lean4-keywords1)))
  (add-hook 'lean4-mode-hook
	    (lambda ()
	      (setq-local defun-prompt-regexp "^\\(example\\|theorem\\|lemma\\)\\>")
	      (setq-local tab-width 2)
	      (setq-local company-minimum-prefix-length 3)
	      (setq-local lsp-enable-file-watchers nil)
	      (setq-local company-backends
			  '((company-capf company-yasnippet company-keywords)
			    company-files
			    company-dabbrev-code
			    company-dabbrev))))
  )

(defun lean4-insert-show ()
  (interactive)
  (lsp-request-async
   "$/lean/plainGoal"
   (lsp--text-document-position-params)
   (-lambda ((ignored &as &lean:PlainGoal? :goals))
     (let* ((fg (aref goals 0))
	    (target (car (last (split-string fg "⊢")))))
       (insert "show " target))
     :error-handler #'ignore
     :mode 'tick
     :cancel-token :plain-goal)))

(defun org-babel-execute:lean4 (body params)
  "Execute a block of lean4 code with org-babel."
  (let ((in-file (org-babel-temp-file "lean4" ".lean"))
	(args (or (cdr (assq :args params)) "")))
    (with-temp-file in-file
      (insert body))
    (org-babel-eval
     (format "lean %s %s" args (org-babel-process-file-name in-file))
     "")))

(provide 'ob-lean4)

(with-eval-after-load 'org
  (add-to-list 'org-babel-load-languages
	       '(lean4 . t)))


(provide 'init-lean4)

;;; init-lean4.el ends here
