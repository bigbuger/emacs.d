;;; my-org.el --- org mode config

;;; Commentary:
;; 

;;; Code:

(require 'org)
(require 'smartparens)
(require 'verb)
(require 'org-mouse)
(require 'ob-go)

(setq org-support-shift-select t)
(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation nil)

(use-package org-superstar
  :init
  (setq org-superstar-remove-leading-stars t)
  :hook
  (org-mode . org-superstar-mode))

(use-package org-fragtog
  :hook
  (org-mode . org-fragtog-mode))

;; (add-hook 'org-mode-hook
;; 	  (lambda ()
;; 	    (when (not (string= (buffer-name) "*scratch*"))
;; 	      (olivetti-mode 1))))
;; (setq olivetti-body-width 220)

(setq org-hide-emphasis-markers t)
(use-package org-appear
  :init
  (add-hook 'org-mode-hook 'org-appear-mode)
  (setq org-appear-autolinks t))

(add-hook 'org-mode-hook
	  (lambda ()
	    "Beautify Org Symbol"
	    (setq prettify-symbols-alist
		  '(("#+BEGIN_SRC" . "📝")
                    ("#+END_SRC" . "∎")
                    ("#+begin_src" . "📝")
                    ("#+end_src" . "∎")
		    ("[ ]" .  "⬜")
		    ("[X]" . "✅")
		    ("[-] . "🟩"")))
	    (prettify-symbols-mode)))

(defun org-babel-execute:passthrough (body params)
  body)

;; json output is json
(defalias 'org-babel-execute:json 'org-babel-execute:passthrough)

(provide 'ob-passthrough)


(setq org-imenu-depth 4
      org-src-fontify-natively t
      org-ellipsis " ⤵ " ;; folding symbol
      org-format-latex-options (plist-put org-format-latex-options :scale 2.5)
      org-preview-latex-default-process 'dvisvgm
      org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-preserve-indentation t)


(setq org-ditaa-jar-path "~/tool/ditaa-0.11.0-standalone.jar")
(setq org-plantuml-exec-mode 'plantuml)

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
   (verb . t)
   (ditaa . t)
   (plantuml . t)
   (sql . t)))


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

;; Always redisplay inline images after executing SRC block
(eval-after-load 'org
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))

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

(use-package org-download
  :ensure t
  :defer t
  :init
  ;; Add handlers for drag-and-drop when Org is loaded.
  (with-eval-after-load 'org
    (org-download-enable)))


(use-package gnuplot
  :ensure t)

(provide 'my-org)

;;; my-org.el ends here
