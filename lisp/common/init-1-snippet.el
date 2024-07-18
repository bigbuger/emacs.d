;;; init-snippet.el --- 文字模版相关工具
;;yasnippet

;;; Commentary:
;; 

;;; Code:

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(require 'yasnippet)
(yas-global-mode 1)

(require 'warnings)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))


(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-expand-list
	try-expand-list-all-buffers
        try-expand-line))
(define-key yas-minor-mode-map [remap dabbrev-expand] 'hippie-expand)

(defun he-fix-string-parens (args)
  "remove extra paren when expanding line in smartparens."
  (let* ((str (car args))
	(last (substring str -1)))
    (if (and smartparens-mode (member last '(")" "}" "\"" "'" "`")))
	(list (substring str 0 -1) (cdr args))
      args)))

(advice-add 'he-substitute-string :filter-args #'he-fix-string-parens)

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

(provide 'init-1-snippet)

;;; init-1-snippet.el ends here
