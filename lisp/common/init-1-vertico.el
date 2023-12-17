;;; init-1-vertico.el --- vertico  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(setq enable-recursive-minibuffers t)

(use-package vertico
  :init
  (vertico-mode)
  (vertico-mouse-mode)

  (defvar +vertico-current-arrow t)

  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((and +vertico-current-arrow
                                                   (not (bound-and-true-p vertico-flat-mode)))
                                              (eql t)))
    (setq cand (cl-call-next-method cand prefix suffix index start))
    (if (bound-and-true-p vertico-grid-mode)
	(if (= vertico--index index)
            (concat #("▶" 0 1 (face vertico-current)) cand)
          (concat #("_" 0 1 (display " ")) cand))
      (if (= vertico--index index)
          (concat
           #(" " 0 1 (display (left-fringe right-triangle vertico-current)))
           cand)
	cand))))

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-package orderless
  :demand t
  :init
  (setq completion-styles '(orderless basic))

  ;; disable orderless for company
  (defun company-completion-styles (capf-fn &rest args)
    (let ((completion-styles '(basic partial-completion)))
      (apply capf-fn args)))
  (with-eval-after-load 'company
    (advice-add 'company-capf :around #'company-completion-styles))
  )


;; 中文拼音搜索
(use-package pyim
  :ensure t
  :init
  (defun pyim-orderless-regexp (orig_func component)
    (let ((result (funcall orig_func component)))
      (pyim-cregexp-build result)))

  (defun toggle-chinese-search ()
    (interactive)
    (if (not (advice-member-p #'pyim-orderless-regexp 'orderless-regexp))
	(advice-add 'orderless-regexp :around #'pyim-orderless-regexp)
      (advice-remove 'orderless-regexp #'pyim-orderless-regexp)))

  (defun disable-py-search (&optional _args)
    (if (advice-member-p #'pyim-orderless-regexp 'orderless-regexp)
	(advice-remove 'orderless-regexp #'pyim-orderless-regexp)))

  (defun enable-py-search (&optional _args)
    (if (not (advice-member-p #'pyim-orderless-regexp 'orderless-regexp))
	(advice-add 'orderless-regexp :around #'pyim-orderless-regexp)))
    
  (add-hook 'minibuffer-exit-hook 'disable-py-search)

  (defun using-py-search (fun)
    (advice-add fun :before (lambda (&rest _args)
			      (enable-py-search))))

  (using-py-search 'find-file)
  (using-py-search 'recentf))


;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :init
  (marginalia-mode)
  (add-to-list 'marginalia-prompt-categories '("\\<file\\>" . file))
  (add-to-list 'marginalia-prompt-categories '("\\<buffer\\>" . buffer)))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(recentf-mode t)

(use-package consult
  :demand t

  :init
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  ;; 支持在 minibuffer 中 调用 completion 时使用 consult-completion-in-region, 例如 `shell-command'
  (setq completion-in-region-function
	(lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
		 args)))
  
  
  (setq consult-narrow-key "C-+") ;; narrow 切换多个分组
  (setq consult-line-start-from-top t)
  
  (global-set-key (kbd "C-x b") 'consult-buffer)
  (global-set-key (kbd "C-c i") 'consult-imenu)
  (global-set-key (kbd "C-c M-y") 'consult-yank-pop)
  (global-set-key (kbd "C-c m") 'consult-bookmark)
  (global-set-key (kbd "C-c s") 'consult-ripgrep)
  (global-set-key (kbd "C-c f") 'consult-fd)
  (global-set-key (kbd "C-c e") 'consult-recent-file)
  (global-set-key (kbd "C-c l") 'consult-line)
  (global-set-key (kbd "C-h C-i") 'consult-info)

  
  (define-key minibuffer-local-map (kbd "C-r") 'consult-history)

  (using-py-search 'consult-line)
  (using-py-search 'consult-recent-file))

;; consult 没有 isearch 支持, 用 isearch-mb 有更好的搜索体验
(use-package isearch-mb
  :init
  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)
  (isearch-mb-mode))

(use-package embark
  :ensure t
  :demand t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   :map minibuffer-mode-map
   ("C-c C-o" . embark-export))

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :init
  (setq which-key-use-C-h-commands nil
        ;; press C-h after a prefix key, it shows all the possible key bindings and let you choose what you want
        prefix-help-command #'embark-prefix-help-command)

  ;; embark display in an bottom buffer
  ;; (setq
  ;;  embark-verbose-indicator-display-action
  ;;  '((display-buffer-at-bottom)
  ;;    (window-parameters (mode-line-format . none))
  ;;    (window-height . 230)))

  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
	(which-key--show-keymap
	 (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
	 (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
	 nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
	'(embark-which-key-indicator
	  embark-highlight-indicator
	  embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))
  
  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))

(eval-when-compile
  (defmacro my-embark-ace-action (fn)
    `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "%s"
	 (require 'ace-window)
	 (let ((aw-dispatch-always t))
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn)))))))

(define-key embark-file-map     (kbd "o") (my-embark-ace-action find-file))
(define-key embark-buffer-map   (kbd "o") (my-embark-ace-action switch-to-buffer))
(define-key embark-bookmark-map (kbd "o") (my-embark-ace-action bookmark-jump))

(eval-when-compile
  (defmacro my-embark-split-action (fn split-type)
    `(defun ,(intern (concat "my/embark-"
			     (symbol-name fn)
			     "-"
			     (car (last  (split-string
					  (symbol-name split-type) "-"))))) ()
       (interactive)
       (funcall #',split-type)
       (call-interactively #',fn))))

(define-key embark-file-map     (kbd "2") (my-embark-split-action find-file split-window-below))
(define-key embark-buffer-map   (kbd "2") (my-embark-split-action switch-to-buffer split-window-below))
(define-key embark-bookmark-map (kbd "2") (my-embark-split-action bookmark-jump split-window-below))

(define-key embark-file-map     (kbd "3") (my-embark-split-action find-file split-window-right))
(define-key embark-buffer-map   (kbd "3") (my-embark-split-action switch-to-buffer split-window-right))
(define-key embark-bookmark-map (kbd "3") (my-embark-split-action bookmark-jump split-window-right))

(defun sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting sudo"))
  (find-file (if (file-remote-p file)
                 (concat "/" (file-remote-p file 'method) ":"
                         (file-remote-p file 'user) "@" (file-remote-p file 'host)
                         "|sudo:root@"
                         (file-remote-p file 'host) ":" (file-remote-p file 'localname))
               (concat "/sudo:root@localhost:" file))))

(define-key embark-file-map (kbd "S") 'sudo-find-file)

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :after (consult embark)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)

  :init
  ;; embark-consult-export-grep 添加路径，不然在没有项目的情况下，导出 grep mode 不识别
  (defun +embark-consult-export-grep (orig-fun &rest args)
    (let* ((lines (car args))
	   (tranfar-lines (mapcar (lambda (l)
				 (concat "./" l))
				lines)))
      (funcall orig-fun tranfar-lines)))

  (advice-add 'embark-consult-export-grep :around '+embark-consult-export-grep)
  
  )

(use-package wgrep
  :bind (:map grep-mode-map
	      ("C-c C-q" . wgrep-change-to-wgrep-mode)))

(use-package consult-lsp
  :after (lsp)
  :init
  (define-key lsp-command-map (kbd "s") 'consult-lsp-symbols))

(use-package vertico-calc
  :load-path "~/.emacs.d/lisp/libs/"
  :bind (("C-c q" . vertico-calc)))


(provide 'init-1-vertico)

;;; init-1-vertico.el ends here
