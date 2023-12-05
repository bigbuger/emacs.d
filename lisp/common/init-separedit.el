;;; init-separedit.el --- separedit setting

;;; Commentary:
;; 

;;; Code:

(use-package separedit
  :ensure t
  :demand t
  :init
  (define-key prog-mode-map        (kbd "C-c '") #'separedit))

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
	     (mode (intern (concat (symbol-name language) "-mode"))))
	(if (fboundp mode)
	    (funcall mode)))))
  
    (add-hook 'separedit-buffer-creation-hook 'my-separedit-guass-mode))

(provide 'init-separedit)

;;; init-separedit.el ends here
