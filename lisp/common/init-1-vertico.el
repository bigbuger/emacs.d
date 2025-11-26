;;; init-1-vertico.el --- vertico  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(setq enable-recursive-minibuffers t)

(use-package vertico
  :bind (:map vertico-map
	      ("C-M-n" . vertico-next-group)
	      ("C-M-p" . vertico-previous-group)
	      ("M-S"   . vertico-toggle-sort)
	      ("s-j"   . vertico-quick-jump)
	      ("C-c j" . vertico-quick-jump))
  
  :init
  (vertico-mode)
  (vertico-multiform-mode 1)
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
	(concat     "  " cand))))

  (defun vertico-toggle-sort ()
    (interactive)
    (setq-local vertico-sort-override-function
		(and (not vertico-sort-override-function)
                     #'vertico-sort-alpha)
		vertico--input t))

  ;; macos 下面 man 获取补全会很慢，所以屏蔽之..
  ;; https://github.com/minad/vertico/issues/297
  (defun macos-man-completion-table-before-while (&rest _)
    "If running under macOS, do not attempt to complete manual page names, 
since the whatis index is broken post-SIP."
    (not (eq system-type 'darwin)))
  (advice-add #'Man-completion-table :before-while #'macos-man-completion-table-before-while)
  )

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-package orderless
  :demand t
  :config
  ;; (setq orderless-style-dispatchers (list #'orderless-kwd-dispatch ; 查询可以通过 :mod:lisp :dir:xxx 之类的命令查询 buffer
  ;;                                         #'orderless-affix-dispatch))
  
  (setq orderless-matching-styles
	'(orderless-literal		; use = to dispatch
	  orderless-regexp		; use % to dispatch
	  ;; orderless-prefixes          ; re-re matches query-replace-regexp, recode-region and magit-remote-list-refs; f-d.t matches final-draft.txt
	  ;; orderless-literal-prefix    ; The component is treated as a literal string that must occur as a prefix of a candidate. use ^ to dispatch , 和正则开头类似？
	  ;; orderless-initialism        ; This maps abc to \<a.*\<b.*\c, use , to dispatch
	  ;; orderless-flex              ; This maps abc to a.*b.*c, use ~ to dispatch
	  ))

  ;; 中文拼音搜索
  (require 'pyim)
  (defun orderless-regex-pinyin (str)
    (pyim-cregexp-build (orderless-regexp str)))
  
  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-literal
				 orderless-regexp
				 orderless-initialism)))
  
  (orderless-define-completion-style orderless+initialism+pinyin
    (orderless-matching-styles '(orderless-literal
				 orderless-regex-pinyin
				 orderless-initialism)))

  (orderless-define-completion-style orderless+initialism+prefixes
    (orderless-matching-styles '(orderless-literal
				 orderless-regexp
				 orderless-prefixes
				 orderless-initialism)))

  (orderless-define-completion-style orderless+pinyin
    (orderless-matching-styles '(orderless-literal
				 orderless-regex-pinyin)))
  
  (setq orderless-component-separator #'orderless-escapable-split-on-space)
  
  ;; use initialism for command, buffer and file
  (setq completion-category-overrides
	'((command (styles orderless+initialism+prefixes))
          (buffer (styles orderless+initialism+pinyin))
	  (file (styles orderless+initialism+pinyin))
	  (project (styles orderless+initialism+pinyin))
	  (project-file (styles orderless+initialism+pinyin))
	  (line (styles orderless+pinyin))
	  (bookmark (styles orderless+initialism+pinyin))))
  
  
  :init
  (defun +orderless-completion-style()
    (setq-local completion-styles '(orderless basic partial-completion emacs22)))
  (add-hook 'minibuffer-setup-hook '+orderless-completion-style)

  (defun using-orderless(orig_fun &rest args)
    "Using orderless for sepecial function."
    (let ((completion-styles '(orderless)))
      (apply orig_fun args))))


;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :init
  (marginalia-mode)
  (setq marginalia-command-categories
        (append '((projectile-find-file . file)
                  (projectile-find-dir . file)
                  (projectile-switch-project . file)
		  (consult-line . line))
                marginalia-command-categories))
  (add-to-list 'marginalia-prompt-categories '("\\<buffer\\>" . buffer)))

(use-package nerd-icons-completion
  :after (marginalia)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  (setq nerd-icons-completion-category-icons (cl-delete-if
					      (lambda (x) (not (car x)))
					      nerd-icons-completion-category-icons )))

(recentf-mode t)

(use-package consult
  :demand t

  :config
  (setf (alist-get 'consult-line vertico-multiform-commands)
	'((vertico-cycle . t)))
  
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
  ;; (setq consult-line-start-from-top t)
  
  (global-set-key (kbd "C-x b") 'consult-buffer)
  (global-set-key (kbd "C-c M-y") 'consult-yank-pop)
  (global-set-key (kbd "C-c m") 'consult-bookmark)
  (global-set-key (kbd "C-c s") 'consult-ripgrep)
  (global-set-key (kbd "C-c f") 'consult-fd)
  (global-set-key (kbd "C-c e") 'consult-recent-file)
  (global-set-key (kbd "C-c l") 'consult-line)
  (global-set-key (kbd "C-c i") 'consult-imenu)
  
  (global-set-key (kbd "M-g i") 'consult-imenu)	      ;; orig. imenu
  (global-set-key (kbd "M-g M-g") 'consult-goto-line) ;; orig. goto-line
  (global-set-key (kbd "M-g g") 'consult-goto-line) ;; orig. goto-line
  ;; (global-set-key (kbd "M-g m") 'consult-mark)
  
  (global-set-key (kbd "M-s k")  'consult-keep-lines)
  (global-set-key (kbd "M-s u")  'consult-focus-lines)
  (global-set-key (kbd "M-s e")  'consult-isearch-history)
  
  (global-set-key (kbd "C-h I") 'consult-info)

  
  (define-key minibuffer-local-map (kbd "M-r") 'consult-history)
  ;; (with-eval-after-load 'visual-regexp
  ;;   (define-key vr/minibuffer-keymap (kbd "M-r") 'consult-history))
  
  (with-eval-after-load "org"
    (define-key org-mode-map (kbd "C-c i") #'consult-outline)
    (define-key org-mode-map (kbd "M-g i") #'consult-outline))


  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (defun consult--get-completion-options-from-help (exec)
    "Generate exec options table vai `exec' -h."
    (when (executable-find exec)
      (let* ((-h (shell-command-to-string (concat exec  " --help")))
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

  (defmacro def-consult-help (command exec)
    (let ((options-fun (intern (format "consult-%s-get-completion-options" exec)))
	  (options-alist (intern (format "consult-%s-completion-options-alist" exec)))
	  (annotion (intern (format "consult-%s-completion-annotation" exec)))
	  (table (intern (format "consult-%s-completion-table" exec)))
	  (capf (intern (format "consult-%s-completion-at-point" exec)))
	  (adv (intern (format "consult-%s-with-completion-at-point" exec))))
      `(progn
	 (defun ,options-fun ()
	     "Generate options table vai -h."
	   (consult--get-completion-options-from-help ,exec))

	 (defcustom ,options-alist
	   (,options-fun)
	   ,(format "%s options alist." exec))
	 
	 (defun ,annotion (candidate)
	   "Annotation for rg option."
	   (cdr (assoc candidate ,options-alist)))
	 
	 (defun ,table ()
	   "List all option for rg."
	   (mapcar #'car ,options-alist))
	 
	 (defun ,capf ()
	   "Completion option.
This is the function to be used for the hook `completion-at-point-functions'."
	   (interactive)
	   (let* ((bds (bounds-of-thing-at-point 'symbol))
		  (start (car bds))
		  (end (cdr bds)))
	     (list start end (,table) :annotation-function #',annotion)))

	 (defun ,adv (orign &rest args)
	   (minibuffer-with-setup-hook
	       (:append
		(lambda ()
		  (add-hook 'completion-at-point-functions
			    #',capf nil t)))
	     (apply orign args)))

	 (advice-add ,command :around ',adv))))
  (def-consult-help 'consult-ripgrep "rg")
  
  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-xref
   consult--source-file-register
   consult-line
   consult-goto-line
   :preview-key '(:debounce 0.4 any) ;; Option 1: Delay preview
   ;; :preview-key "M-."             ;; Option 2: Manual preview
   )
  
  (consult-customize
   consult-bookmark
   consult-recent-file
   consult--source-recent-file
   consult--source-project-recent-file
   consult--source-bookmark
   consult-theme
   consult-buffer
   consult--source-buffer
   
   :preview-key '("M-.")))

(use-package consult
  :after orderless
  :defines consult-line-literal
  
  :config
  ;; Use the `orderless` completion style, restricted to `orderless-literal`
  (defun consult-line-literal ()
    (interactive)
    (let ((completion-styles '(orderless))
          (orderless-matching-styles '(orderless-literal))
          (completion-category-defaults nil)
          (completion-category-overrides nil))
      (consult-line)))
  
  (advice-add #'consult-focus-lines :around #'using-orderless)
  (advice-add #'consult-keep-lines :around #'using-orderless)

  ;; 修正 "$" 匹配行结尾
  ;; https://github.com/minad/consult/wiki#orderless-style-dispatchers-ensure-that-the--regexp-works-with-consult-buffer
  (defun orderless-fix-dollar-dispatch (word &optional _index _total)
    (concat word (if (boundp 'consult--tofu-regexp)
                     (concat consult--tofu-regexp "*$")
                   "$")))

  (defun +orderless-fix-dollar (orign &rest args)
    (minibuffer-with-setup-hook
	(lambda ()
	  (setq-local orderless-affix-dispatch-alist
		      `((?% . ,#'char-fold-to-regexp)
			(?! . ,#'orderless-not)
			;; (?& . ,#'orderless-annotation)
			(?, . ,#'orderless-initialism)
			(?= . ,#'orderless-literal)
			;; (?^ . ,#'orderless-literal-prefix) ;; 正则行首，不要用来做 dispatch
			(?~ . ,#'orderless-flex)
			(?$ . orderless-fix-dollar-dispatch))))
      (apply orign args)))
  (advice-add #'consult-line :around #'+orderless-fix-dollar)

  ;; Use Orderless as pattern compiler for consult-grep/ripgrep/find
  (defun consult--orderless-regexp-compiler (input type &rest _config)
    (setq input (cdr (orderless-compile input)))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input t str))))

  ;; OPTION 1: Activate globally for all consult-grep/ripgrep/find/...
  (setq consult--regexp-compiler #'consult--orderless-regexp-compiler)

  ;; OPTION 2: Activate only for some commands, e.g., consult-ripgrep!
  ;; (defun consult--with-orderless (&rest args)
  ;;   (minibuffer-with-setup-hook
  ;;     (lambda ()
  ;;       (setq-local consult--regexp-compiler #'consult--orderless-regexp-compiler))
  ;;     (apply args)))
  ;; (advice-add #'consult-ripgrep :around #'consult--with-orderless)

  ;; fd --full-path 会包含文件名，所以会导致用当前目录/项目名去搜索是有问题，这里给正则加上开头为当前目录，大部分情况可以适配掉
  (defun consult-fd--orderless-regexp-compiler (input type &rest _config)
    (setq input (cdr (orderless-compile input)))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp (concat "^" default-directory  ".*" r) type)) input)
     (lambda (str) (orderless--highlight input t str))))

  (defun consult-fd--with-orderless (&rest args)
    (minibuffer-with-setup-hook
	(lambda ()
          (setq-local consult--regexp-compiler #'consult-fd--orderless-regexp-compiler))
      (apply args)))
  (advice-add #'consult-fd :around #'consult-fd--with-orderless)
  (def-consult-help 'consult-fd "fd")
  )

(use-package consult
  :after projectile
  :defines consult-buffer-sources
  :config
  (projectile-load-known-projects)
  (setq my-consult-source-projectile-projects
        `(:name "Projectile projects"
                :narrow   ?p
                :category file
                :action   ,#'projectile-switch-project-by-name
                :items    ,(lambda () (projectile-known-projects))))
  
  (setq consult-buffer-sources '(consult--source-buffer
				 consult--source-hidden-buffer
				 consult--source-modified-buffer
				 consult--source-other-buffer
				 consult--source-recent-file
				 consult--source-buffer-register
				 ;; consult--source-file-register
				 consult--source-bookmark
				 consult--source-project-buffer-hidden
				 consult--source-project-recent-file-hidden
				 my-consult-source-projectile-projects))
  
  )

;; consult 没有 isearch 支持, 用 isearch-mb 有更好的搜索体验
(use-package isearch-mb
  :bind (("C-s" . isearch-forward-regexp)
	 ("C-r" . isearch-backward-regexp))

  :config
  (add-to-list 'isearch-mb--with-buffer #'isearch-yank-word)
  (add-to-list 'isearch-mb--after-exit #'isearch-occur)
  (define-key isearch-mb-minibuffer-map (kbd "C-w") #'isearch-yank-word)

  :init
  (isearch-mb-mode))

(use-package embark
  :ensure t
  :demand t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding

   :map embark-identifier-map
   ("c" . string-inflection-lower-camelcase)
   ("C" . string-inflection-camelcase)
   ("u" . string-inflection-underscore)
   ("U" . string-inflection-upcase)
   ;; ("k" . string-inflection-kebab-case)

   :map minibuffer-mode-map
   ("C-c C-o" . embark-export)
   ("C-c C-l" . embark-collect)
   ("C-c C-S-l" . embark-live)
   ("C-<return>" . embark-dwim-noquit)
   :map embark-general-map
   ("C-c C-o" . embark-export)
   ("C-c C-l" . embark-collect)
   ("C-c C-S-l" . embark-live)

   :map embark-collect-mode-map
   ("<SPC>" . embark-select)

   :map embark-consult-async-search-map
   ("f" . consult-fd)

   :map vertico-map
   ("C-<SPC>" . embark-select)
   ("M-C-<return>" . embark-act-all))

  :config
  (defvar embark-collect-smart-column-regex
    "consult-lsp-diagnostics")
  
  (add-hook 'embark-collect-mode-hook
	    #'(lambda ()
		(unless (string-match-p
			 embark-collect-smart-column-regex
			 (buffer-name))
		  (setq tabulated-list-format
			[("Candidate" 140 t) ("Annotation" 0 t)]))))
  (setq embark-confirm-act-all nil)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (unbind-key "S" embark-general-map) ;; embark-collect
  (unbind-key "E" embark-general-map) ;; embark-export
  (unbind-key "L" embark-general-map) ;; embark-live
  
  (unbind-key "C" embark-general-map) 	;; embark-consult-search-map
  (define-key embark-general-map (kbd "C-c") 'embark-consult-search-map)
  (unbind-key "r" embark-consult-search-map) ;; #'consult-ripgrep
  (define-key embark-consult-search-map "s" #'consult-ripgrep) ;; #'consult-ripgrep

  ;; (unbind-key "e" embark-region-map) ;; #'eval-region
  (unbind-key "f" embark-region-map) ;; #'fill-region
  (unbind-key "p" embark-region-map) ;; #'fill-region-as-paragraph
  (unbind-key "$" embark-region-map) ;; #'ispell-region
  (unbind-key "o" embark-region-map) ;; #'org-table-convert-region
  (unbind-key "+" embark-region-map) ;; #'append-to-file
  (unbind-key "*" embark-region-map) ;; #'calc-grab-region
  (unbind-key ":" embark-region-map) ;; #'calc-grab-sum-down
  (unbind-key "_" embark-region-map) ;; #'calc-grab-sum-across
  (unbind-key "b" embark-region-map) ;; #'browse-url-of-region
  (unbind-key "h" embark-region-map) ;; #'shr-render-region
  (unbind-key "'" embark-region-map) ;; #'expand-region-abbrevs
  (unbind-key "v" embark-region-map) ;; #'vc-region-history
  (unbind-key "R" embark-region-map) ;; #'repunctuate-sentences

  ;; make all map use same bind, N for narrow
  (unbind-key "n" embark-region-map) ;; #'narrow-to-region
  (define-key embark-region-map "N" #'narrow-to-region)

  (define-key embark-become-file+buffer-map "p" #'projectile-find-file)

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
    `(defun ,(intern (concat "my-embark-ace-" (symbol-name fn))) ()
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
    `(defun ,(intern (concat "my-embark-"
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

(require 'ace-window)
(defun embark-ace-insert(strings)
  "Insert `STRING' into select window by `ace-window'."
  (if (length< (aw-window-list) 2)
      (embark-insert strings)
    (let ((aw-dispatch-always t))
      (aw-select " Ace - Insert"
		 #'(lambda (window)
		     (with-selected-window window
		       (if buffer-read-only
			   (message "Buffer is read-only")
			 (embark-insert strings))))))))
(add-to-list 'embark-multitarget-actions #'embark-ace-insert)
(define-key embark-general-map (kbd "i") #'embark-ace-insert)

;; i for insert
(unbind-key "i" embark-package-map) ;; #'package-install
(define-key embark-package-map "i" #'embark-insert)
(define-key embark-package-map "I" #'package-install)


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

(use-package vertico-posframe
  :config
  (setq vertico-posframe-truncate-lines nil))

(provide 'init-1-vertico)

;;; init-1-vertico.el ends here
