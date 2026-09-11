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
(unbind-key "TAB" clutch-mode-map)


(with-eval-after-load 'flycheck
  (defun flycheck-parse-sqlfluff (output checker buffer)
    "Parse SQLFluff JSON output (nested by file) into Flycheck errors.
Output example:`
[
  {
    \"filepath\": \"test.sql\",
    \"violations\": [
      {
        \"start_line_no\": 3,
        \"start_line_pos\": 1,
        \"code\": \"PRS\",
        \"description\": \"Line 3, Position 1: Found unparsable section: 'where b=1'\",
        \"name\": \"\",
        \"warning\": false,
        \"start_file_pos\": 28,
        \"end_line_no\": 3,
        \"end_line_pos\": 10,
        \"end_file_pos\": 37
      }
    ],
    \"statistics\": {
      \"source_chars\": 39,
      \"templated_chars\": 39,
      \"segments\": 45,
      \"raw_segments\": 28
    },
    \"timings\": {
      \"templating\": 0.0018279999999999963,
      \"lexing\": 0.000623874999999996,
      \"parsing\": 0.006174459000000021,
      \"linting\": 0.0002551249999999672
    }
  }
]
'
"
    (flatten-list
     (mapcar
      (lambda (node)
	(mapcar
	 (lambda (item)
           (flycheck-error-new-at
            (gethash "start_line_no" item)
            (gethash "start_line_pos" item)
            'error
            (gethash "description" item)
            :id (gethash "code" item)
            :end-line (gethash "end_line_no" item)
            :end-column (gethash "end_line_pos" item)
            :checker checker
            :buffer buffer))
	 (gethash "violations" node)))
      (json-parse-string output))))
  
  (defvar sql-product->sqlfluff-dialect
    '((mysql       . "mysql")
      (postgresql  . "postgres")
      (sqlite      . "sqlite")
      (oracle      . "oracle")
      (db2         . "db2")
      (ansi        . "ansi"))
    "Map Emacs `sql-product' symbols to SQLFluff dialect names.")

  (defun sqlfluff-dialect-for-buffer ()
    "Return the SQLFluff dialect string for the current buffer's `sql-product'."
    (let ((product (or sql-product 'ansi)))  ; 默认回退到 ansi
      (or (cdr (assq product sql-product->sqlfluff-dialect))
          "ansi")))
  
  (flycheck-define-checker sql-sqlfluff
    "A MySQL SQL checker using sqlfluff."
    :command ("sqlfluff" "lint" "--dialect" (eval (sqlfluff-dialect-for-buffer)) "--format" "json" "--exclude-rules" "all" "-")
    :standard-input t
    :error-parser flycheck-parse-sqlfluff
    :modes (sql-mode clutch-mode))

  (add-to-list 'flycheck-checkers 'sql-sqlfluff))

(defun align-sql-insert-values ()
  (interactive)
  (align-regexp (region-beginning) (region-end)
		"\\s-*\\(\\s\".*?\\s\"\\|NULL\\|[0-9]+\\|`.*?`\\|[a-zA-Z0-9_]+\\)\\(\\s-*,\\s-*\\)"
		-2 1 t))

(provide 'init-databaseclient)

;;; init-databaseclient.el ends here
