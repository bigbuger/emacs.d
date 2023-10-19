;;; global.el --- global config

;;; Commentary:
;;

;;; Code:


(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(setq initial-major-mode 'org-mode)
(setq initial-scratch-message
      "# This buffer is for text that is not saved, and for Org-mode.
# To create a file, visit it with <open> and enter text in its buffer.

")

(require 'saveplace)
(add-hook 'after-init-hook
	  (save-place-mode))

(setq Man-notify-method 'pushy)

;; 关闭启动画面
(setq inhibit-startup-message t)

;; 关闭蜂鸣
(setq visible-bell t)
;; alway hight light
(global-font-lock-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)


;; 保存命令历史
(savehist-mode 1)

(auto-image-file-mode)

;; 临时 buffer 根据后缀选择 major-mode
(setq-default major-mode
	      (lambda () (if buffer-file-name
			     (fundamental-mode)
			   (let ((buffer-file-name (buffer-name)))
			     (set-auto-mode)))))

;;(global-linum-mode 1)
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode +1)))
(column-number-mode 1)

;; Auto delete region when typing
(delete-selection-mode 1)

;; disable bookmark fringe
(setq bookmark-set-fringe-mark nil)

;; whitch function mode
(which-function-mode)

(add-hook 'prog-mode-hook
	  (lambda ()
	    (setq header-line-format
		  '((which-func-mode ("" which-func-format " "))))))
(setq mode-line-misc-info
      ;; We remove Which Function Mode from the mode line, because it's mostly
      ;; invisible here anyway.
      (assq-delete-all 'which-function-mode mode-line-misc-info))

(add-hook 'prog-mode-hook
	  (lambda ()
	    (font-lock-add-keywords nil
				    '(("\\<\\(FIXME\\|fixme\\|TODO\\|todo\\|BUG\\|bug\\)\\>" 1 font-lock-warning-face t)))))

;; enable narrow
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; make it split horizontal
(setq split-width-threshold 1000)

;; base key ========================================
;;关闭当前缓冲区 Alt+4  ;; C-x 0
(global-set-key (kbd "M-4") 'delete-window)
;;关闭其它缓冲区 Alt+1  ;; C-x 1
(global-set-key (kbd "M-1") 'delete-other-windows)
;;水平分割缓冲区 Alt+2  ;; C-x 2
(global-set-key (kbd "M-2") 'split-window-vertically)
;;垂直分割缓冲区 Alt+3  ;; C-x 3
(global-set-key (kbd "M-3") 'split-window-horizontally)

					;全屏函数
(defun toggle-fullscreen (&optional f)
  "Fullscreen."
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))
(global-set-key [f11] 'toggle-fullscreen)

(require 'crux)
(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key (kbd "C-^") 'crux-top-join-line)
(global-set-key (kbd "C-d") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
(global-set-key (kbd "C-c M-r") #'crux-rename-file-and-buffer)

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-auto-save-history nil)

;;括号匹配
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)


(require 'smartparens-config)
(add-hook 'prog-mode-hook #'smartparens-mode)
(sp-local-pair 'emacs-lisp-mode "'" nil :actions '(wrap))
(global-set-key (kbd "C-}") 'sp-slurp-hybrid-sexp)
(define-key emacs-lisp-mode-map  (kbd "C-}") 'sp-forward-slurp-sexp)
(global-set-key (kbd "C-{") 'sp-forward-barf-sexp)
(global-set-key (kbd "C-c <backspace>") 'sp-raise-sexp)
(global-set-key (kbd "C-M-a") 'sp-beginning-of-sexp)
(global-set-key (kbd "C-M-e") 'sp-end-of-sexp)
(global-set-key (kbd "C-M-d") 'sp-clone-sexp)
(define-key smartparens-mode-map (kbd "C-M-<up>") 'sp-up-sexp)

(defun sp-up-sexp-and-new-line (&optional arg)
  "Jump to end of the sexp the point is in and insert newline.
ARG is pass to `sp-end-of-sexp'"
  (interactive)
  (progn
    (sp-up-sexp)
    (newline-and-indent)))
(global-set-key (kbd "C-<return>") 'sp-up-sexp-and-new-line)

(require 'cl-lib)
(defmacro def-pairs (pairs)
  `(progn
     ,@(cl-loop for (key . val) in pairs
		collect
		`(defun ,(read (concat
				"wrap-with-"
				(prin1-to-string key)
				"s"))
		     (&optional arg)
		   (interactive "p")
		   (sp-wrap-with-pair ,val)))))

(def-pairs ((paren . "(")
	    (bracket . "[")
	    (brace . "{")
	    (single-quote . "'")
	    (double-quote . "\"")
	    (back-quote . "`")))
(global-set-key (kbd "C-c (") 'wrap-with-parens)
(global-set-key (kbd "C-c [") 'wrap-with-brackets)
(global-set-key (kbd "C-c {") 'wrap-with-braces)
(global-set-key (kbd "C-c '") 'wrap-with-single-quotes)
(global-set-key (kbd "C-c \"") 'wrap-with-double-quotes)
(global-set-key (kbd "C-c `") 'wrap-with-back-quotes)

(require 'move-text)
(move-text-default-bindings)
(defun indent-region-advice (&rest ignored)
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))

(advice-add 'move-text-up :after 'indent-region-advice)
(advice-add 'move-text-down :after 'indent-region-advice)


(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


(require 'string-inflection)
(global-set-key (kbd "C-c u") 'string-inflection-toggle)
(defhydra hydra-string-inflection (:hint nil :exit t)
  "
string inflection
_c_: lower-camelcase fooVar  ^_C_: camelcase FooBar
_u_: underscore foo__bar      _U_: upcase FOO__BAR
_k_: kebab foo-bar          ^ _q_: cancel.
"
  ("c" string-inflection-lower-camelcase)
  ("C" string-inflection-camelcase)
  ("u" string-inflection-underscore)
  ("U" string-inflection-upcase)
  ("k" string-inflection-kebab-case)
  ("q" nil))
(global-set-key (kbd "C-c M-u") 'hydra-string-inflection/body)

(require 'auto-highlight-symbol)
(with-eval-after-load 'auto-highlight-symbol
  (setq auto-highlight-symbol-mode-map (make-sparse-keymap)))
(add-hook 'auto-highlight-symbol-mode-hook
	  #'(lambda ()
	      (assq-delete-all 'auto-highlight-symbol-mode minor-mode-map-alist)))

(global-auto-highlight-symbol-mode t)

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
(global-set-key (kbd "C-c g") 'counsel-rg)
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

;; (require 'ivy-posframe)
;; (setq ivy-display-function #'ivy-posframe-display)
;; (setq ivy-display-function #'ivy-posframe-display-at-frame-center)
;; (setq ivy-display-function #'ivy-posframe-display-at-window-center)
;; (setq ivy-display-function #'ivy-posframe-display-at-frame-bottom-left)
;; (setq ivy-display-function #'ivy-posframe-display-at-window-bottom-left)
;; (setq ivy-display-function #'ivy-posframe-display-at-point)
;; (ivy-posframe-enable)

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
(setq yas/root-directory "~/.emacs.d/snippets")
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
;;(define-key projectile-command-map (kbd "p") 'counsel-projectile-switch-project)
;;(define-key projectile-command-map (kbd "a") 'counsel-projectile-ag)
(define-key projectile-command-map (kbd "g") 'counsel-projectile-rg)
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
(global-set-key (kbd "C-c C-g") 'magit-dispatch)
(require 'magit-todos)
(magit-todos-mode)

(setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
(setq magit-git-output-coding-system 'utf-8-unix)
(add-to-list 'magit-process-password-prompt-regexps
	     ".*verification code: ?$")

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


;; 自动保存
(add-to-list 'load-path "~/.emacs.d/lisp/auto-save/")
(require 'auto-save)            ;; 加载自动保存模块
(auto-save-enable)              ;; 开启自动保存功能
(setq auto-save-silent t)       ;; 自动保存的时候静悄悄的， 不要打扰我
(setq auto-save-disable-predicates
      '((lambda ()
	  (not (file-writable-p (buffer-file-name)))) ;; 不可写文件不自动保存
	(lambda ()
	  (tramp-tramp-file-p (buffer-file-name))) ;; tramp 模式不自动保存
	(lambda ()
	  (string-prefix-p "*" (buffer-file-name)))
	(lambda ()
	  (string-prefix-p "#" (buffer-file-name)))))


;; visual-regexp
(require 'visual-regexp)
(global-set-key (kbd "C-c r") 'vr/replace)
(global-set-key (kbd "C-c q") 'vr/query-replace)

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

(add-to-list 'load-path "~/.emacs.d/lisp/flycheck-posframe")
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

(add-to-list 'load-path "~/.emacs.d/lisp/lsp-ivy")
(require 'lsp-ivy)
(setq lsp-ivy-show-symbol-filename nil)
(define-key lsp-command-map (kbd "s") 'lsp-ivy-workspace-symbol)

(with-eval-after-load "lsp-mode"
  (add-to-list 'lsp-disabled-clients 'semgrep-ls))

;; end of lsp setting


;; vterm
(require 'vterm)
(global-set-key (kbd "C-c s") 'vterm-other-window)
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

(define-key projectile-command-map (kbd "v") 'projectile-run-vterm-other-window)

(use-package multi-vterm :ensure t)

;; compile
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(define-key menu-bar-tools-menu [compile] '("Compile..." . smart-compile))
(define-key prog-mode-map (kbd "s-r") 'smart-compile)
(with-eval-after-load 'projectile
  (define-key projectile-command-map (kbd "r") 'projectile-run-project))


;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-c M-i") 'mc/insert-numbers)
(define-key mc/keymap (kbd "C-c C-g") 'mc/keyboard-quit)


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


(add-to-list 'load-path "~/.emacs.d/lisp/company-english-helper")
(require 'company-english-helper)
(global-set-key (kbd "<C-M-tab>") 'company-english-helper-search)


;; centaur-tabs
(require 'centaur-tabs)
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-style "alternate")

(setq centaur-tabs-set-bar 'under)
;; Note: If you're not using Spacmeacs, in order for the underline to display
;; correctly you must add the following line:
(setq x-underline-at-descent-line t)
(setq centaur-tabs-show-navigation-buttons t)
(setq centaur-tabs-forward-tab-text " ▸ ")
(setq centaur-tabs-backward-tab-text " ◂ ")
(setq centaur-tabs-down-tab-text  " ↯ ")


(defun centaur-tabs-hide-tab (x)
  "Do no to show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer name not match below blacklist.
     ;; just block all buffer start with *
     (and (string-prefix-p "*" name)
	  (not (with-current-buffer x (derived-mode-p 'comint-mode)))
	  (not (with-current-buffer x (derived-mode-p 'compilation-mode)))
	  (not (string-prefix-p "*compilation" name))
	  (not (string-prefix-p "*vterm" name))
	  (not (string-prefix-p "*term" name))
	  (not (string-prefix-p "*shell" name))
	  (not (string-prefix-p "*ielm" name)))

     ;; Is not temp version
     (string-match-p "^.+\\.~.+~$" name)


     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name)))
     )))

(defun centaur-tabs-buffer-groups ()
  "`centaur-tabs-buffer-groups' control buffers' group rules."
  (list
   (cond

    ((or (derived-mode-p 'eshell-mode)
	 (derived-mode-p 'term-mode)
	 (derived-mode-p 'shell-mode)
	 (derived-mode-p 'vterm-mode)
	 (derived-mode-p 'compilation-mode)
	 (derived-mode-p 'comint-mode))
     "Execute")

    ((derived-mode-p 'dired-mode)
     "Dired")

    ((and (not (projectile-project-root))
	  (memq major-mode '(org-mode org-agenda-mode diary-mode)))
     "OrgMode")

    ((or (string-equal "*" (substring (buffer-name) 0 1))
	 (memq major-mode '(magit-process-mode
			    magit-status-mode
			    magit-diff-mode
			    magit-log-mode
			    magit-file-mode
			    magit-blob-mode
			    magit-blame-mode
			    )))
     "Emacs")

    (t
     (centaur-tabs-get-group-name (current-buffer))))))


(add-hook 'after-init-hook
	  (lambda ()
	    (centaur-tabs-mode t)))

(with-eval-after-load 'counsel
  (global-set-key (kbd "C-c t") 'centaur-tabs-counsel-switch-group))

;; about indent
(require 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)


;; about dired
;; (require 'all-the-icons-dired)
;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(setq dired-dwim-target t)
(setq wdired-allow-to-change-permissions t)
(setq delete-by-moving-to-trash t)
(setq dired-listing-switches "-l -h --time-style=long-iso")

(require 'osx-trash)

(when (eq system-type 'darwin)
  (setq insert-directory-program "gls")
  (osx-trash-setup))

(with-eval-after-load 'dired
  (require 'dired-x)
  ;; Set dired-x global variables here.  For example:
  ;; (setq dired-guess-shell-gnutar "gtar")
  ;; (setq dired-x-hands-off-my-keys nil)
  )

(setq dired-omit-files "\\`[.][.]?\\'")
(setq dired-omit-extensions nil)

(add-hook 'dired-mode-hook
	  (lambda ()
	    ;; Set dired-x buffer-local variables here.  For example:
	    (dired-omit-mode 1)
	    ))

(require 'dirvish)
(dirvish-override-dired-mode)
(setq dirvish-attributes '(vc-state all-the-icons subtree-state file-size))
(setq dirvish-mode-line-format nil)

(define-key dirvish-mode-map
  (kbd "TAB") 'dirvish-subtree-toggle)
(define-key dirvish-mode-map
  (kbd "C-c t") 'dirvish-layout-toggle)
(define-key dirvish-mode-map
  (kbd ".") 'dired-create-empty-file)
(define-key dirvish-mode-map
  (kbd "/") 'dirvish-fd)

(add-hook 'dirvish-find-entry-hook
	  (lambda (&rest _) (setq-local truncate-lines t)))

(setq dirvish-reuse-session t)

;; hideshowvis
(add-to-list 'load-path "~/.emacs.d/lisp/hideshowvis/")
(require 'hideshowvis)
(hideshowvis-symbols)
;;(add-hook 'prog-mode-hook 'hideshowvis-enable)

;; graphviz-dot
(require 'graphviz-dot-mode)

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
