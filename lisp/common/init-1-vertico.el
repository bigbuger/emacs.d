;;; init-1-vertico.el --- vertico  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(setq enable-recursive-minibuffers t)

(use-package vertico
  :bind (:map vertico-map
	      ("C-M-n" . vertico-next-group)
	      ("C-M-p" . vertico-previous-group))
  
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
            (concat #("➤ " 0 1 (face vertico-current)) cand)
          (concat #("_" 0 1 (display " ")) cand))
      (if (= vertico--index index)
          (concat #("➤ " 0 2 (face vertico-current)) cand)
	(concat     "  " cand)))))

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-package orderless
  :demand t
  :config
  (setq orderless-matching-styles
	'(orderless-literal
	  orderless-regexp
	  ;; orderless-prefixes          ; f-d.t matches final-draft.txt
	  ;; orderless-initialism        ; This maps abc to \<a.*\<b.*\c
	  ;; orderless-flex              ; This maps abc to a.*b.*c
	  ))

  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-literal
				 orderless-regexp
				 orderless-initialism)))
  (setq orderless-component-separator #'orderless-escapable-split-on-space)
  
  ;; use initialism for command, buffer and file
  (setq completion-category-overrides
	'((command (styles orderless+initialism))
          (buffer (styles orderless+initialism))
	  (file (styles orderless+initialism))
	  (project-file (styles orderless+initialism))))
  
  
  :init
  (defun +orderless-completion-style()
    (setq-local completion-styles '(orderless basic partial-completion emacs22)))
  (add-hook 'minibuffer-setup-hook '+orderless-completion-style)

  (defun using-orderless(orig_fun &rest args)
    "Using orderless for sepecial function."
    (let ((completion-styles '(orderless)))
      (apply orig_fun args))))


;; 中文拼音搜索
(use-package pyim
  :ensure t
  :after (orderless)
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
    
  (defun using-py-search (fun &rest args)
      "使用 pyim 进行中文搜索"
      (enable-py-search)
      (apply fun args)
      (disable-py-search))

  (advice-add 'find-file :around #'using-py-search)
  (advice-add 'recentf :around #'using-py-search))


;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :init
  (marginalia-mode)
  (setq marginalia-command-categories
        (append '((projectile-find-file . project-file)
                  (projectile-find-dir . project-file)
                  (projectile-switch-project . file))
                marginalia-command-categories))
  (add-to-list 'marginalia-prompt-categories '("\\<buffer\\>" . buffer)))

(use-package nerd-icons-completion
  :after (marginalia)
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
  (global-set-key (kbd "C-c M-y") 'consult-yank-pop)
  (global-set-key (kbd "C-c m") 'consult-bookmark)
  (global-set-key (kbd "C-c s") 'consult-ripgrep)
  (global-set-key (kbd "C-c f") 'consult-fd)
  (global-set-key (kbd "C-c e") 'consult-recent-file)
  (global-set-key (kbd "C-c l")  'consult-line)
  
  (global-set-key (kbd "M-g i") 'consult-imenu) ;; orig. imenu
  (global-set-key (kbd "M-g M-g") 'consult-goto-line) ;; orig. goto-line
  (global-set-key (kbd "M-g g") 'consult-goto-line) ;; orig. goto-line
  
  (global-set-key (kbd "M-s k")  'consult-keep-lines)
  (global-set-key (kbd "M-s f")  'consult-focus-lines)
  
  (global-set-key (kbd "C-h I") 'consult-info)

  
  (define-key minibuffer-local-map (kbd "C-r") 'consult-history)

  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "M-g i") #'consult-outline))
  
  (advice-add 'consult-line :around #'using-py-search)
  (advice-add 'consult-recent-file :around #'using-py-search)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (defun consult-rg-get-completion-options ()
    "Generate rg options table vai rg -h."
    (when (executable-find "rg")
      (let* ((-h (shell-command-to-string "rg -h"))
	     (-h-list (string-split -h "\\(\\.\\|:\\)\n"))
	     (doc-left-pad 30))
	(mapcan (lambda (h)
		  (let ((l (string-replace "\n" "" h)))
		    (when (string-match (rx-to-string
					  '(: bol (* space)
					      (group "-" (? "-") (+ (or alnum "-")))
					      (? ", ") (? (group "-" (? "-") (+ (or alnum "-"))))
					      (? "=" (+ (or "_" "-" alnum)))
					      (+ space)
					      (group (* any)) eol))
					l)
		      (let* ((short (match-string 1 l))
			     (long (match-string 2 l))
			     (doc (match-string 3 l))
			     (s-pad (- doc-left-pad (length short)))
			     (l-pad (when long (- doc-left-pad (length long))))
			     (s-doc (concat (make-string s-pad ?\s) doc))
			     (l-doc (when long (concat (make-string l-pad ?\s) doc))))
			(if long
			    (list `(,short . ,s-doc)
				  `(,long . ,l-doc))
			  (list `(,short . ,s-doc)))))))
		-h-list))))

  (defcustom consult-rg-completion-options-alist (consult-rg-get-completion-options)
    "Rg options alist.")

  (defun consult-rg-completion-annotation (candidate)
    "Annotation for rg option."
    (cdr (assoc candidate consult-rg-completion-options-alist)))

  (defun consult-rg-completion-table ()
    "List all option for rg."
    (mapcar #'car consult-rg-completion-options-alist))

  (defun consult-rg-completion-at-point ()
    "Completion rg option.
This is the function to be used for the hook `completion-at-point-functions'."
    (interactive)
    (let* ((bds (bounds-of-thing-at-point 'symbol))
           (start (car bds))
           (end (cdr bds)))
      (list start end (consult-rg-completion-table) :annotation-function #'consult-rg-completion-annotation)))

  (defun consult-rg-with-completion-at-point (orign &rest args)
    (minibuffer-with-setup-hook
      (:append
       (lambda ()
	 (add-hook 'completion-at-point-functions
		   #'consult-rg-completion-at-point nil t)))
    (apply orign args)))

  (advice-add 'consult-ripgrep :around #'consult-rg-with-completion-at-point)
  
  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any) ;; Option 1: Delay preview
   ;; :preview-key "M-."             ;; Option 2: Manual preview
   )
  
  (consult-customize consult-theme
                     :preview-key
                     '("M-."))

  (advice-add #'consult-focus-lines :around #'using-orderless)
  (advice-add #'consult-keep-lines :around #'using-orderless))

(use-package consult
  :after projectile
  :defines consult-buffer-sources
  :config
  (projectile-load-known-projects)
  (setq my-consult-source-projectile-projects
        `(:name "Projectile projects"
                :narrow   ?p
                :category project
                :action   ,#'projectile-switch-project-by-name
                :items    ,projectile-known-projects))
  (add-to-list 'consult-buffer-sources my-consult-source-projectile-projects 'append)
  
  )

;; consult 没有 isearch 支持, 用 isearch-mb 有更好的搜索体验
(use-package isearch-mb
  :bind (("C-s" . isearch-forward-regexp)
	 ("C-r" . isearch-backward-regexp))

  :config
  (add-to-list 'isearch-mb--with-buffer #'isearch-yank-word)
  (define-key isearch-mb-minibuffer-map (kbd "C-w") #'isearch-yank-word)

  :init
  (isearch-mb-mode))

(use-package embark
  :ensure t
  :demand t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   :map minibuffer-mode-map
   ("C-c C-o" . embark-export)
   ("C-c C-l" . embark-collect)
   ("C-<return>" . embark-dwim-noquit)
   :map embark-collect-mode-map
   ("<SPC>" . embark-select))

  :config
  (add-hook 'embark-collect-mode-hook
	    #'(lambda ()
		(setq tabulated-list-format
		      [("Candidate" 140 t) ("Annotation" 0 t)])))
  (setq embark-confirm-act-all nil)
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

  (defun embark-dwim-noquit ()
    "Run action but don't quit the minibuffer afterwards."
    (interactive)
    (let ((embark-quit-after-action nil))
      (embark-dwim)))
  
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


(use-package consult-omni
  :load-path "~/.emacs.d/lisp/libs/consult-omni"
  :after consult
  :bind ("C-c j" . consult-omni)
  :init
  (add-to-list 'load-path "~/.emacs.d/lisp/libs/consult-omni/sources")
  ;; Load Sources Core code
  (require 'consult-omni-sources)
  ;; Load Embark Actions
  (require 'consult-omni-embark)
  (setq consult-omni-sources-modules-to-load
	'(consult-omni-ripgrep consult-omni-fd consult-omni-buffer consult-omni-calc consult-omni-wikipedia))
  (consult-omni-sources-load-modules)
;;; set multiple sources for consult-omni-multi command. Change these lists as needed for different interactive commands. Keep in mind that each source has to be a key in `consult-omni-sources-alist'.
  (setq consult-omni-multi-sources '("calc"
                                     "Buffer"
                                     "Bookmark"
				     "ripgrep"
				     "fd"
                                     ;; "File"
                                     ;; "Apps"
                                     ;; "gptel"
                                     ;; "Brave"
                                     ;; "Dictionary"
                                     ;; "Google"
                                     ;; "Wikipedia"
                                     ;; "elfeed"
                                     ;; "mu4e"
                                     ;; "buffers text search"
                                     ;; "Notes Search"
                                     ;; "Org Agenda"
                                     ;; "GitHub"
                                     ;; "YouTube"
                                     ;; "Invidious"
				     ))

  (with-eval-after-load 'projectile
    (defun with-projectile-root (orig-fun &rest args)
      (let ((pr (projectile-project-root)))
	(if pr
	    (let ((default-directory pr))
	      (apply orig-fun args))
	  (apply orig-fun args))))
    (advice-add 'consult-omni-fd :around #'with-projectile-root)
    (advice-add 'consult-omni :around #'with-projectile-root))
  )


(provide 'init-1-vertico)

;;; init-1-vertico.el ends here
