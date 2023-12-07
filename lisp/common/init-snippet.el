;;; init-snippet.el --- 文字模版相关工具
;;yasnippet

;;; Commentary:
;; 

;;; Code:

(setq yas/root-directory '("~/.emacs.d/snippets"))
(require 'yasnippet)
(yas-global-mode 1)
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

(provide 'init-snippet)

;;; init-snippet.el ends here
