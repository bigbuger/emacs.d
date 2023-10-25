;;; my-org.el --- org mode config

;;; Commentary:
;; 

;;; Code:

(require 'org)
(require 'smartparens)
(require 'verb)
(require 'org-mouse)
(require 'ob-go)

(setq org-image-actual-width nil)
(setq org-support-shift-select t)
(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation nil)
(setq org-preview-latex-image-directory "~/.emacs.d/.org/ltximg/")

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
		  '(("#+title:" . "¶")
		    ("#+TITLE:" . "¶")
		    ("tags:" . "🏷️")
		    ("#+BEGIN_SRC" . "📝")
                    ("#+END_SRC" . "∎")
                    ("#+begin_src" . "📝")
                    ("#+end_src" . "∎")
		    ("[ ]" .  "⬜")
		    ("[X]" . "✅")
		    ("[-] . "🟩"")))
	    (prettify-symbols-mode)))

(require 'org-colored-text)
(org-add-link-type
 "color"
 (lambda (path)
   (message (concat "color "
                    (progn (add-text-properties
                            0 (length path)
                            (list 'face `((t (:foreground ,path))))
                            path) path))))
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "<span style=\"color:%s;\">%s</span>" path desc))
    ((eq format 'latex)
     (format "{\\color{%s}%s}" path desc)))))

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

(setq org-file-apps
      (append (mapcar (lambda (ext)
			(cons (concat "\\." ext "\\'")
			      'default))
		      image-file-name-extensions)
	      org-file-apps))

;; Always redisplay inline images after executing SRC block
(eval-after-load 'org
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))


(use-package org-download
  :ensure t
  :defer t
  :bind (:map org-mode-map
	      ("C-M-y" . org-download-clipboard))
  
  :config
  (setq org-download-annotate-function (lambda (_link) ""))
  (setq-default org-download-image-dir "./image")
  (setq org-download-image-attr-list
        '("#+ATTR_ORG: :width 80% :align center"))

  
  :init
  ;; Add handlers for drag-and-drop when Org is loaded.
  (with-eval-after-load 'org
    (org-download-enable)))


(use-package gnuplot
  :ensure t)

(use-package org-roam
  :ensure t
  :init
  (progn
    (setq org-directory (file-truename "~/note/roam"))
    (setq org-roam-directory org-directory)
    (setq org-id-locations-file (concat org-directory "/.org-id-locations"))
    (setq org-roam-capture-templates '(("d" "default" plain "%?" :target
					(file+head "${slug}.org" "#+title: ${title}\n#+filetags: \n")
					:unnarrowed t))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:50} "(propertize "${tags:100}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  ;; (require 'org-roam-protocol)
  )

(use-package org-roam-ui
  :ensure t
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :bind (("C-c n u" . org-roam-ui-open))
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t
	org-roam-ui-browser-function #'xwidget-webkit-browse-url))

(use-package org-ql)


(provide 'my-org)

;;; my-org.el ends here
