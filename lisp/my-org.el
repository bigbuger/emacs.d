;;; my-org.el --- org mode config

;;; Commentary:
;; 

;;; Code:

(require 'org)
(require 'smartparens)
(require 'org-bullets)
(require 'verb)
(require 'org-mouse)
(require 'ob-go)

(setq org-support-shift-select t)
;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; (add-hook 'org-mode-hook
;; 	  (lambda ()
;; 	    (when (not (string= (buffer-name) "*scratch*"))
;; 	      (olivetti-mode 1))))
;; (setq olivetti-body-width 220)

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
   (perl . t)
   (python . t)
   (haskell . t)
   (shell . t)
   (go . t)
   (js . t)
   (passthrough . t)
   (latex . t)
   (dot . t)
   (restclient . t)
   (verb . t)))

;; latex company
(add-hook 'org-mode-hook
          (lambda () (setq-local company-backends
				 (cl-adjoin '(company-math-symbols-latex :with company-yasnippet) company-backends :test #'equal))))

(add-hook 'org-mode-hook #'smartparens-mode)
(sp-local-pair 'org-mode "\\[" "\\]")

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(setq org-confirm-babel-evaluate nil)

;; inline显示图片
(setq org-startup-with-inline-images 1)

(defun org-insert-image ()
  (interactive)
  (let* ((path (concat default-directory "img/"))
	 (image-file (concat
		      path
		      (buffer-name)
		      (format-time-string "_%Y%m%d_%H%M%S.png"))))
    (if (not (file-exists-p path))
	(mkdir path))
    (shell-command (concat "pngpaste " image-file))
    (org-insert-link nil (concat "file:" image-file) ""))
  (org-display-inline-images))

(provide 'my-org)

;;; my-org.el ends here
