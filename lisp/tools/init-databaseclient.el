;;; init-databaseclient.el --- database client setting
;;;###autoload

;;; Commentary:
;; 

;;; Code:


(use-package sql
  :defer t
  :init
  (custom-set-variables
   '(sql-mysql-login-params '(server databases port user password)))
  :init
  (setq lsp-sqls-workspace-config-path nil)
  ;; :hook
  ;; (sql-mode . lsp)
  )

(use-package sqlformat
  :ensure t)

(add-to-list 'load-path "~/.emacs.d/lisp/libs/clutch")
(add-to-list 'load-path "~/.emacs.d/lisp/libs/mysql.el")   ; only for :backend mysql
(add-to-list 'load-path "~/.emacs.d/lisp/libs/mongodb.el") ; only for :backend mongodb
(add-to-list 'load-path "~/.emacs.d/lisp/libs/redis.el")   ; only for :backend redis
(require 'clutch)

(defun align-sql-insert-values ()
  (interactive)
  (align-regexp (region-beginning) (region-end)
		"\\s-*\\(\\s\".*?\\s\"\\|NULL\\|[0-9]+\\|`.*?`\\|[a-zA-Z0-9_]+\\)\\(\\s-*,\\s-*\\)"
		-2 1 t))

(provide 'init-databaseclient)

;;; init-databaseclient.el ends here
