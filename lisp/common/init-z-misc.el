;;; init-misc.el --- 不知道怎么分类的配置🤣



;;; Commentary:
;; 

;;; Code:

;; dumb-jump 跳转到代码定义
(require 'dumb-jump)
(setq dumb-jump-force-searcher 'rg)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(with-eval-after-load 'c-mode
  (unbind-key "C-c ." 'c-mode-base-map))

(with-eval-after-load 'protobuf-mode
  (unbind-key "C-c ." 'protobuf-mode-map))

(global-set-key (kbd "C-c .") 'dumb-jump-go)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

;; which-key 快捷键打小抄
(require 'which-key)
(which-key-mode)
;; 不要重新排序，按照按键绑定的先后顺序就行
;; (setq which-key-sort-order nil)


;; imenu-list 侧边栏显示 imenu
(require 'imenu-list)
(global-set-key (kbd "<f9>") 'imenu-list)
(setq imenu-auto-rescan t)

;; helpful 更好的帮助文档
(require 'helpful)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)
(global-set-key (kbd "C-h f") #'helpful-function)
(global-set-key (kbd "C-h v") #'helpful-variable)

(define-key emacs-lisp-mode-map (kbd "C-c C-d") #'helpful-at-point)


;; dash doc 查 dash 文档
(use-package consult-dash
  :demand t
  :bind (("M-s d" . consult-dash))
  :config
  ;; Use the symbol at point as initial search term
  (consult-customize consult-dash :initial (thing-at-point 'symbol))
  (setq dash-docs-docsets-path "~/.docset")
  (setq dash-docs-enable-debugging nil)
  (setq dash-docs-browser-func
	#'(lambda (url &rest args)
	    (xwidget-webkit-browse-url url args)
	    (display-buffer xwidget-webkit-last-session-buffer)))
  
  :init
  (setq-default consult-dash-docsets '("Redis" "MySql" "MongoDB" "SQLite")))

;; ace-window 快速通过数字切换到指定窗口
(require 'ace-window)
(global-set-key (kbd "C-c o") 'ace-window)

;; nerd-icons-ibuffer
(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))


;; osx-dictionary
(require 'pos-tip)
(setq pos-tip-tab-width 800)
(require 'osx-dictionary)
(defun osx-dictionary-search-at-point-and-pop ()
  "Search word around and display result with popup."
  (interactive)
  (let* ((word (osx-dictionary--region-or-word))
	 (result (osx-dictionary--search word)))
    (pos-tip-show result)))
(global-set-key (kbd "C-S-d") 'osx-dictionary-search-at-point-and-pop)

;; google 翻译
(require 'google-translate)
(require 'google-translate-default-ui)
(setq google-translate-default-source-language "auto")
(setq google-translate-default-target-language "zh-CN")

;; flyspell
(setq flyspell-mark-duplications-flag nil)
(require 'flyspell-correct-popup)
(define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)

(setq ispell-program-name "aspell")
;; (setq ispell-extra-args '("--sug-mode=ultra" "--camel-case"))

;; Jinx is a fast just-in-time spell-checker for Emacs.
(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (unbind-key "<down-mouse-3>" jinx-overlay-map) ;; disable mouse
  (put 'jinx-overlay 'mouse-face nil)            ;; disable mouse hover face
  
  (setq jinx-exclude-regexps
	'((emacs-lisp-mode "Package-Requires:.*$")
	  (t "[A-Z]+\\>"         ;; Uppercase words
	     "-+\\>"             ;; Hyphens used as lines or bullet points
	     "\\w*?[0-9]\\w*\\>" ;; Words with numbers, hex codes
	     "[a-z]+://\\S-+"    ;; URI
	     "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?" ;; Email
	     "\\(?:Local Variables\\|End\\):\\s-*$" ;; Local variable indicator
	     "jinx-\\(?:languages\\|local-words\\):\\s-+.*$" ;; Local variables
	     "\\cc" ;; Chinese
	     ".*/.*\\>" ;; File path
	     "\\<[a-zA-Z]\\{1,3\\}\\>" ;; short word
	     ))
	)
  
  (setq jinx-include-faces
	'((prog-mode font-lock-comment-face font-lock-doc-face font-lock-string-face font-lock-constant-face
		     font-lock-variable-name-face)
	  (conf-mode font-lock-comment-face font-lock-string-face)

	  ;; `yaml-mode' and `yaml-ts-mode' are text-modes,
	  ;; while they should better be conf- or prog-modes.
	  (yaml-mode . conf-mode)
	  (yaml-ts-mode . conf-mode)))

  (defvar jinx-min-word-length 3
    "Jinx check mint length.")
  
  (defun my-jinx--ignore-case-word-valid-p (start)
    "Return non-nil if word, that is assumed to be in lower case, at
START is valid, or would be valid if capitalized or upcased."
    (let ((word (buffer-substring-no-properties start (point))))
      (or (member word jinx--session-words)
	  (<= (length word) jinx-min-word-length)
	  (cl-loop for dict in jinx--dicts thereis
		   (or
		    (jinx--mod-check dict (upcase word))
		    (jinx--mod-check dict (downcase word))
		    (jinx--mod-check dict (capitalize word))
		    (jinx--mod-check dict word))))))

  (setq jinx--predicates
	(list #'jinx--face-ignored-p
              #'jinx--regexp-ignored-p
	      #'my-jinx--ignore-case-word-valid-p
              #'jinx--word-valid-p))
  
  )

(use-package vertico
  :config
  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20))))

;; end flyspell

;; vlf 打开文件
(use-package vlf
  :init
  (require 'vlf-setup))

;; calc 菜单
(use-package casual
  :after calc
  :ensure t
  :bind (:map calc-mode-map ("C-o" . 'casual-calc-tmenu)))

(require 're-builder)
(setq reb-re-syntax 'string)
(use-package casual
  :ensure t
  :bind (:map reb-mode-map ("C-o" . 'casual-re-builder-tmenu)
	      :map reb-lisp-mode-map ("C-o" . 'casual-re-builder-tmenu)
	      :map reb-subexp-mode-map ("C-o" . 'casual-re-builder-tmenu)))

;; consult-dasel 多后端搜索
(use-package consult-dasel
  :load-path "~/.emacs.d/lisp/libs/"
  :init
  (with-eval-after-load 'conf-mode
    (define-key conf-toml-mode-map (kbd "C-c C-j") #'consult-dasel)))

;; 项目模版系统
(use-package skeletor
  :ensure t
  :custom
  (skeletor-completing-read-function #'completing-read-default) ;; use default, aka: ivy or vertical or what every activate.
  :bind (("s-n" . skeletor-create-project-at)))


;; (use-package dimmer
;;   :init
;;   (dimmer-configure-which-key)
;;   (dimmer-mode t))

;; EMT stands for Emacs MacOS Tokenizer.
;; This package use macOS’s built-in NLP tokenizer to tokenize and operate on CJK words in Emacs.
(use-package emt
  :load-path "~/.emacs.d/lisp/libs/emt"
  :hook (after-init . emt-mode))

(require 'point-undo)
(global-set-key (kbd "s-[") #'point-undo)
(global-set-key (kbd "s-]") #'point-redo)

(use-package pcre2el)

;; pcmpl-args extends option and argument completion of shell commands read by Emacs.
;; It is intended to make shell completion in Emacs comparable to the rather excellent completion provided by both Bash and Zsh.
(use-package pcmpl-args
  :init
  (defalias 'pcomplete/rg 'pcmpl-args-pcomplete-on-help))

(provide 'init-z-misc)

;;; init-z-misc.el ends here
