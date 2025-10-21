;;; init-other-tools.el ---


;; dash doc 查 dash 文档
;;; Code:

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

;; casual 菜单
(use-package casual
  :after calc
  :ensure t
  :bind (:map calc-mode-map ("C-o" . 'casual-calc-tmenu)))


;;; Commentary:
;; 

(require 're-builder)
(setq reb-re-syntax 'string)
(use-package casual
  :ensure t
  :bind (:map reb-mode-map ("C-o" . 'casual-re-builder-tmenu)
	      :map reb-lisp-mode-map ("C-o" . 'casual-re-builder-tmenu)
	      :map reb-subexp-mode-map ("C-o" . 'casual-re-builder-tmenu)))

(use-package casual
  :ensure t
  :bind (:map isearch-mode-map ("C-o" . 'casual-isearch-tmenu)))

(use-package isearch-mb
  :config
  (define-key isearch-mb-minibuffer-map (kbd "C-o") #'casual-isearch-tmenu))

;; EMT stands for Emacs MacOS Tokenizer.
;; This package use macOS’s built-in NLP tokenizer to tokenize and operate on CJK words in Emacs.
(use-package emt
  :load-path "~/.emacs.d/lisp/libs/emt"
  :hook (after-init . emt-mode))

(provide 'init-other-tools)

;;; init-other-tools.el ends here
