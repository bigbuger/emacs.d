;;; init-other-tools.el --- other tools

;;; Commentary:
;;

;;; Code:

;; 调用 rg 搜索, 我一般用 consult-rg，不过有时候它的 regex 会被各种转义, 所以留了这个
(require 'rg)
(rg-define-search my-rg-project
  :query ask
  :format regexp
  :files "everything"
  :confirm prefix
  :dir project)
(global-set-key (kbd "M-s r") #'rg-menu)
(add-hook 'rg-mode-hook #'hl-line-mode)


;; dash doc 查 dash 文档
(use-package consult-dash
  :demand t
  :bind (("M-s d" . consult-dash))
  :config
  ;; Use the symbol at point as initial search term
  (consult-customize consult-dash :initial (thing-at-point 'symbol))
  (setq dash-docs-docsets-path "~/.docset"
	dash-docs-enable-debugging nil
	dash-docs-browser-func #'browse-url)

  :init
  (setq-default consult-dash-docsets '("Redis" "MySql" "MongoDB" "SQLite")))

;; casual 菜单
(use-package casual
  :after calc
  :ensure t
  :bind (:map calc-mode-map ("C-o" . 'casual-calc-tmenu)))

(require 're-builder)
(setq reb-re-syntax 'string)
(use-package casual
  :ensure t
  :bind ( :map reb-mode-map        ("C-o" . 'casual-re-builder-tmenu)
	  :map reb-lisp-mode-map   ("C-o" . 'casual-re-builder-tmenu)
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

(use-package format-all
  :bind (:map prog-mode-map
	      ("C-M-l" . format-all-region-or-buffer))
  :commands (format-all-mode format-all-region-or-buffer)
  :config
  (add-hook 'prog-mode-hook #'format-all-ensure-formatter))


;; 时间戳 <-> 字符串
(defun my-convert-time (timestamp)
  "Convert TIMESTAMP to iso8601 and put it into kill ring."
  (interactive "ntimestamp:")
  (let ((time-string (format-time-string "%FT%T%z" (seconds-to-time timestamp))))
    (progn
      (kill-new time-string)
      (message "date is: %s" time-string))))
(defalias 'd2t 'my-convert-time)

(defun my-get-timestamp (time-string)
  "Convert TIME-STRING to unix timestamp and put it into kill ring."
  (interactive "stime-string:")
  (let* ((tl0 (parse-time-string time-string))
	 (tl-with-out-zone (butlast tl0))
	 (tl (append (mapcar #'(lambda (i) (if i i 0)) tl-with-out-zone)
		     (last tl0)))
	 (et (apply #'encode-time tl))
	 (time-stamp (format-time-string "%s" et)))
    (progn
      (kill-new time-stamp)
      (message "timestamp is: %s" time-stamp))))
(defalias 't2d 'my-get-timestamp)


(provide 'init-other-tools)

;;; init-other-tools.el ends here
