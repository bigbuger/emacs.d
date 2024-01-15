;;; init-separedit.el --- separedit setting

;;; Commentary:
;; 

;;; Code:

(use-package separedit
  :ensure t
  :demand t
  :config
  (add-to-list 'separedit-string-quotes-alist
	       '(ruby-mode "\"\"\"" "'''" "\"" "'" "`"))
  :init
  (defun bind-separedit-key()
    (local-set-key (kbd "C-c '") #'separedit))
  (add-hook 'prog-mode-hook 'bind-separedit-key))

(use-package language-detection
  :ensure t
  :demand t

  :init
  (defun my-separedit-guass-mode()
    "Guass buffer's code language using `language-detection-buffer'.
And then switch to prefer mode."
    (when (derived-mode-p 'separedit-single-quote-string-mode
                          'separedit-double-quote-string-mode)
      (let* ((language (language-detection-buffer))
	     (mode (intern (concat (symbol-name language) "-mode")))
	     (header-line-format-back header-line-format))
	(if (fboundp mode)
	    (progn
	      (funcall mode)
	      (setq-local header-line-format header-line-format-back))))))
  
    (add-hook 'separedit-buffer-creation-hook 'my-separedit-guass-mode))

(provide 'init-z-separedit)

;;; init-z-separedit.el ends here
