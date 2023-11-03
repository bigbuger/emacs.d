;;; global.el --- global config

;;; Commentary:
;;

;;; Code:

(require 'imenu-list)
(global-set-key (kbd "<f9>") 'imenu-list)
(setq imenu-auto-rescan t)

;;ivy
(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq enable-recursive-minibuffers t)
(setq ivy-use-selectable-prompt t)
(setq ivy-display-style 'fancy)
(setq counsel-fzf-cmd "fd --type f --hidden --follow --exclude .git --color never '%s'")

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-S-s") 'swiper-isearch)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
;;(global-set-key (kbd "<f1> k") 'counsel-descbinds)
(global-set-key (kbd "C-c M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-c i") 'counsel-imenu)
(global-set-key (kbd "C-c e") 'counsel-recentf)
(global-set-key (kbd "C-c b") 'counsel-ibuffer)
(global-set-key (kbd "C-c s") 'counsel-rg)
(global-set-key (kbd "C-c f") 'counsel-fzf)
;;(global-set-key (kbd "C-c j") 'counsel-file-jump)
(global-set-key (kbd "C-c m") 'counsel-bookmark)
(global-set-key (kbd "C-c c") 'counsel-git-checkout)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)


(require 'counsel-fd)
;;(global-set-key (kbd "C-c j") 'counsel-fd-file-jump)
(require 'dired-x)
(global-set-key (kbd "C-c j") 'counsel-fd-dired-jump)


(require 'ivy-rich)
(setq ivy-rich-path-style 'abbrev)
(setq ivy-rich-display-transformers-list
      (append ivy-rich-display-transformers-list
	      '(counsel-bookmark
		(:columns
		 ((ivy-rich-bookmark-type)
		  (ivy-rich-candidate (:width 10))
		  (ivy-rich-bookmark-info))))))
(all-the-icons-ivy-rich-mode 1)
(ivy-rich-mode 1)


(require 'pyim-cregexp-utils)
(setq ivy-re-builders-alist
      '((t . pyim-cregexp-ivy)))

(use-package ivy-posframe
  :ensure t
  :init
  (setq ivy-posframe-display-functions-alist
	'((lsp-execute-code-action . ivy-posframe-display-at-point)
          (t                       . ivy-display-function-fallback)))
  :init
  (ivy-posframe-mode 1))

;; end of ivy

(require 'helpful)
(setq counsel-describe-function-function #'helpful-callable)
(setq counsel-describe-variable-function #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)

(define-key emacs-lisp-mode-map (kbd "C-c C-d") #'helpful-at-point)

(require 'which-key)
(which-key-mode)

(require 'ace-window)
(global-set-key (kbd "C-c o") 'ace-window)

;;yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(setq yas/root-directory '("~/.emacs.d/snippets"))
;;(yas/load-directory yas/root-directory)
(global-set-key (kbd "<M-RET>") 'yas-expand)

(require 'shell)
(dolist (hook (list
               'term-mode-hook
	       'shell-mode-hook
               ))
  (add-hook hook #'(lambda () (yas-minor-mode -1))))

(require 'auto-yasnippet)
(global-set-key (kbd "C-S-w") #'aya-create)
(global-set-key (kbd "C-S-y") #'aya-expand)

(require 'yatemplate)
(setq auto-insert-query nil)
(setq auto-insert-alist nil)
(yatemplate-fill-alist)
(auto-insert-mode t)


;;company
(require 'company)
(global-company-mode 1)
(setq company-minimum-prefix-length 1)
(setq company-require-match 'never)
(setq company-show-quick-access t)
(add-hook 'after-init-hook 'company-quickhelp-mode)

(global-set-key (kbd "<backtab>") 'company-complete)
(global-set-key (kbd "<C-S-tab>") 'company-files)
;; (global-set-key (kbd "<C-M-tab>") 'company-ispell)
(add-to-list 'load-path "~/.emacs.d/lisp/libs/company-english-helper")
(require 'company-english-helper)
(global-set-key (kbd "<C-M-tab>") 'company-english-helper-search)

(setq company-backends
      '(company-semantic
	company-cmake
	company-capf
	company-clang
	company-files
	(company-dabbrev-code company-gtags company-etags company-keywords)))

(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  "Add company-yasnippet after BACKEND."
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
(setq company-backends (append company-backends '(company-yasnippet)))


;;projectile
(require 'projectile)
(projectile-mode)
(setq projectile-require-project-root t)
(setq projectile-indexing-method 'hybrid)
(setq projectile-completion-system 'ivy)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

(require 'counsel-projectile)
(define-key projectile-command-map (kbd "s") 'counsel-projectile-rg)
(define-key projectile-command-map (kbd "b") 'counsel-projectile-switch-to-buffer)
(define-key projectile-command-map (kbd "f") 'counsel-projectile-find-file)


(setq frame-title-format
      '(""
	"%b"
	(:eval
	 (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format " in [%s]" project-name))))))


;; ibuffer
(require 'ibuffer-projectile)
(defun ibuffer-projectile-filter ()
  "Set up `ibuffer-projectile'."
  (ibuffer-projectile-set-filter-groups)
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic)))

(add-hook 'ibuffer-hook #'ibuffer-projectile-filter)


(require 'all-the-icons-ibuffer)
(add-hook 'ibuffer-mode-hook #'all-the-icons-ibuffer-mode)


;; treemacs
(require 'treemacs)
(require 'treemacs-all-the-icons)
(treemacs-load-theme "all-the-icons")

(setq treemacs-follow-after-init t)
(treemacs-project-follow-mode)
(global-set-key [f8] 'treemacs)
(global-set-key (kbd "M-0") 'treemacs-select-window)

(setq treemacs-filewatch-mode t)
(setq treemacs-file-event-delay 50)



;; magit
(require 'magit)
(global-set-key (kbd "C-c g") ' magit-file-dispatch)
(global-set-key (kbd "C-c C-g") 'magit-dispatch)
(require 'magit-todos)
(magit-todos-mode)

(setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
(setq magit-git-output-coding-system 'utf-8-unix)
(add-to-list 'magit-process-password-prompt-regexps
	     ".*verification code: ?$")

(push '(margin
	(margin-format    . ("%C %a %s%f"))
	(margin-width     . 42)
	(margin-face      . magit-blame-margin)
	(margin-body-face . (magit-blame-dimmed)))
      magit-blame-styles)

(require 'magit-delta)
;; (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))

;; ediff
(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)
(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)
(setq ediff-show-ancestor nil)

;; diff-hl
(require 'diff-hl)
(global-diff-hl-mode)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(defhydra hydra-diff-hl (global-map "C-c v")
  "vc"
  ("n" diff-hl-next-hunk "next hunk")
  ("p" diff-hl-previous-hunk "previous hunk")
  ("r" diff-hl-revert-hunk "revert hunk")
  ("q" nil "exit"))





;; dumb-jump
(require 'dumb-jump)
(setq dumb-jump-force-searcher 'rg)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(global-set-key (kbd "C-.") 'dumb-jump-go)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)



;; flycheck
;;(eval-after-load 'flymake '(require 'flymake-cursor))
(require 'flycheck)
(global-flycheck-mode)

(add-to-list 'load-path "~/.emacs.d/lisp/libs/flycheck-posframe")
(require 'flycheck-posframe)
(setq flycheck-posframe-warning-prefix "⚠ ")
(setq flycheck-posframe-info-prefix "ℹ ")
(setq flycheck-posframe-error-prefix "⨯ ")

(setq flycheck-posframe-timeout 4)
(with-eval-after-load 'flycheck
  (require 'flycheck-posframe)
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(setq flycheck-indication-mode 'right-fringe)

(defvar-local flycheck-local-checkers nil)
(defun +flycheck-checker-get(fn checker property)
  (or (alist-get property (alist-get checker flycheck-local-checkers))
      (funcall fn checker property)))
(advice-add 'flycheck-checker-get :around '+flycheck-checker-get)

(setq flycheck-checker-error-threshold 600)


;; realgud
(require 'realgud)
(load-library "realgud")

;; lsp setting
(require 'lsp-mode)
;;(require 'company-lsp)
(require 'lsp-ui)
(require 'dap-mode)

(setq lsp-auto-guess-root t)
(setq lsp-prefer-flymake :none)
(setq lsp-ui-flycheck-enable t)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(setq lsp-headerline-breadcrumb-enable t)
;; (add-hook 'lsp-mode-hook
;; 	  '(lambda ()
;; 	     (setq-local header-line-format
;; 			 '((t
;; 			    (:eval
;; 			     (frame-parameter nil 'lsp-headerline--string)))))))

(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-position 'at-point)
(setq lsp-ui-doc-show-with-cursor nil)

(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-sideline-show-code-actions t)

(setq lsp-ui-doc-include-signature t)

(setq lsp-signature-auto-activate nil)
(setq lsp-signature-render-documentation nil)
(setq lsp-signature-function 'lsp-signature-posframe)
(define-key lsp-mode-map (kbd "s-d") 'lsp-signature-activate)
(define-key lsp-mode-map (kbd "s-l l") 'lsp-ui-sideline-mode)

(setq lsp-auto-execute-action nil)
(setq lsp-modeline-code-actions-segments '(count icon))

(setq lsp-completion-provider :none)
(add-hook 'lsp-completion-mode-hook
	  '(lambda ()
	     (setq-local company-backends
			 (cl-adjoin '(company-capf :separate company-yasnippet)
				    company-backends :test #'equal))))

(setq dap-auto-configure-features '(locals controls tooltip))
(define-key lsp-mode-map (kbd "M-?") 'lsp-ui-peek-find-references)
(define-key lsp-mode-map [f5] 'dap-debug)
(define-key lsp-mode-map (kbd "C-<f5>") 'dap-hydra)
;; (add-hook 'dap-stopped-hook
;;           (lambda (arg) (call-interactively #'dap-hydra)))


(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(add-to-list 'load-path "~/.emacs.d/lisp/libs/lsp-ivy")
(require 'lsp-ivy)
(setq lsp-ivy-show-symbol-filename nil)
(define-key lsp-command-map (kbd "s") 'lsp-ivy-workspace-symbol)

(with-eval-after-load "lsp-mode"
  (add-to-list 'lsp-disabled-clients 'semgrep-ls))

;; end of lsp setting


;; vterm
(require 'vterm)
(global-set-key (kbd "C-c t") 'vterm-other-window)
(add-to-list 'vterm-eval-cmds '("find-file-other-window" find-file-other-window))

(defun projectile-run-vterm-other-window (&optional arg)
  "Invoke `vterm-other-window' in the project's root.

Switch to the project specific term buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead."
  (interactive "P")
  (let* ((project (projectile-acquire-root))
         (buffer (projectile-generate-process-name "vterm" arg project)))
    (unless (buffer-live-p (get-buffer buffer))
      (unless (require 'vterm nil 'noerror)
        (error "Package 'vterm' is not available"))
      (projectile-with-default-dir project
        (vterm-other-window buffer)))
    (switch-to-buffer buffer)))

(define-key projectile-command-map (kbd "t") 'projectile-run-vterm-other-window)

(use-package multi-vterm :ensure t)

;; compile
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(define-key menu-bar-tools-menu [compile] '("Compile..." . smart-compile))
(define-key prog-mode-map (kbd "s-r") 'smart-compile)
(with-eval-after-load 'projectile
  (define-key projectile-command-map (kbd "r") 'projectile-run-project))


;; restclient
(require 'restclient)
(require 'company-restclient)
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
(add-hook 'restclient-mode-hook
          (lambda () (setq-local company-backends
				 (cl-adjoin '(company-restclient :with company-yasnippet) company-backends :test #'equal))))


;; docker
(require 'docker)
(setq docker-container-shell-file-name "/bin/bash")

;; osx-dictionary
(require 'pos-tip)
(require 'osx-dictionary)
(defun osx-dictionary-search-at-point-and-pop ()
  "Search word around and display result with popup."
  (interactive)
  (let* ((word (osx-dictionary--region-or-word))
	 (result (osx-dictionary--search word)))
    (pos-tip-show result)))
(global-set-key (kbd "C-S-d") 'osx-dictionary-search-at-point-and-pop)

(require 'google-translate)
(require 'google-translate-default-ui)
(setq google-translate-default-source-language "auto")
(setq google-translate-default-target-language "zh-CN")



;; flyspell
(setq flyspell-mark-duplications-flag nil)


(require 'flyspell-correct-popup)
(define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)

(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra" "--camel-case"))


;; dash doc
(require 'counsel-dash)
(setq dash-docs-enable-debugging nil)
(setq counsel-dash-common-docsets '("Redis" "MySQL"))
(setq counsel-dash-docsets-path  (expand-file-name "~/.docset/"))
(setq counsel-dash-browser-func
      #'(lambda (url &rest args)
	  (xwidget-webkit-browse-url url args)
	  (display-buffer xwidget-webkit-last-session-buffer)))
(define-key prog-mode-map (kbd "C-c C-d") 'counsel-dash-at-point)


;; rmsbolt
(require 'rmsbolt)

(with-eval-after-load 'rmsbolt
  (add-to-list 'display-buffer-alist
	       `(,rmsbolt-output-buffer
		 display-buffer-in-direction
		 (direction . right)
		 (window-width . 0.5))))


;; gitlab
(use-package lab :ensure t)

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (custom-set-variables
   '(pdf-tools-handle-upgrades t)))


(provide 'global)

;;; global.el ends here
