;;; init-misc.el --- 不知道怎么分类的配置🤣



;;; Commentary:
;; 

;;; Code:

;; dumb-jump 跳转到代码定义
(require 'dumb-jump)
(setq dumb-jump-force-searcher 'rg)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
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
  (setq-default consult-dash-docsets '("Redis" "MySql")))

;; ace-window 快速通过数字切换到指定窗口
(require 'ace-window)
(global-set-key (kbd "C-c o") 'ace-window)

;; all-the-icons-ibuffer
(require 'all-the-icons-ibuffer)
(add-hook 'ibuffer-mode-hook #'all-the-icons-ibuffer-mode)


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
(setq ispell-extra-args '("--sug-mode=ultra" "--camel-case"))

;; end flyspell

;; restclient 发起 http 请求
(require 'restclient)
(require 'company-restclient)
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
(add-hook 'restclient-mode-hook
          (lambda () (setq-local company-backends
				 (cl-adjoin '(company-restclient :with company-yasnippet) company-backends :test #'equal))))


;; vlf 打开文件
(use-package vlf
  :init
  (require 'vlf-setup))

;; calc 菜单
(use-package casual
  :ensure t
  :bind (:map calc-mode-map ("C-o" . 'casual-main-menu)))

(use-package consult-dasel
  :load-path "~/.emacs.d/lisp/libs/"
  :init
  (with-eval-after-load 'conf-mode
    (define-key conf-toml-mode-map (kbd "C-c C-j") #'consult-dasel)))

(use-package skeletor
  :ensure t
  :custom
  (skeletor-completing-read-function #'completing-read-default) ;; use default, aka: ivy or vertical or what every activate.
  :bind (("s-n" . skeletor-create-project-at)))

(provide 'init-z-misc)

;;; init-z-misc.el ends here
