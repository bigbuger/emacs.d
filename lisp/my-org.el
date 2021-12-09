;;; my-org.el --- org mode config

;;; Commentary:
;; 

;;; Code:

(require 'org)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;;(add-hook 'org-mode-hook (lambda () (olivetti-mode 1)))

(defun org-babel-execute:passthrough (body params)
  body)

;; json output is json
(defalias 'org-babel-execute:json 'org-babel-execute:passthrough)

(provide 'ob-passthrough)

(setq org-src-fontify-natively t
      org-ellipsis " ⤵ " ;; folding symbol
      org-format-latex-options (plist-put org-format-latex-options :scale 2.0))


(setq org-imenu-depth 4)

(org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
	(calc . t)
	(scheme . t)
        (ruby . t)
        (python . t)
	(haskell . t)
        (shell . t)
	(js . t)
	(passthrough . t)
        (latex . t)))

;; latex company
(add-to-list 'company-backends 'company-math-symbols-latex)

(provide 'my-org)

;;; my-org.el ends here
