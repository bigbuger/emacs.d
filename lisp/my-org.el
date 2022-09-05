;;; my-org.el --- org mode config

;;; Commentary:
;; 

;;; Code:

(require 'org)
(require 'smartparens)
(require 'org-bullets)
(require 'verb)


;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook
	  (lambda ()
	    (when (not (string= (buffer-name) "*scratch*"))
		(olivetti-mode 1))))
(setq olivetti-body-width 85)
(setq olivetti-style 'fancy)

(defun org-babel-execute:passthrough (body params)
  body)

;; json output is json
(defalias 'org-babel-execute:json 'org-babel-execute:passthrough)

(provide 'ob-passthrough)


(setq org-imenu-depth 4
      org-src-fontify-natively t
      org-ellipsis " ⤵ " ;; folding symbol
      org-format-latex-options (plist-put org-format-latex-options :scale 2.0)
      org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-preserve-indentation t)


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
        (latex . t)
	(dot . t)
	(restclient . t)
	(verb . t)))

;; latex company
(add-to-list 'company-backends 'company-math-symbols-latex)

(add-hook 'org-mode-hook #'smartparens-mode)
(sp-local-pair 'org-mode "\\[" "\\]")

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(setq org-confirm-babel-evaluate nil)

(provide 'my-org)

;;; my-org.el ends here
