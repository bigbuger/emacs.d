;;; init-ejc.el --- emacs ejc-sql config

;;; Commentary:
;; 

(require 'ejc-sql)
(require 'ejc-company)
(require 'ejc-eldoc)
(require 'sqlformat)

;;; Code:

(setq nrepl-sync-request-timeout 60)
(setq clomacs-httpd-default-port 8090) ; Use a port other than 8080.
;; Allow use any CIDER nREPL not only library dedicated nREPL
;; (setq clomacs-allow-other-repl t)

;; Show results of SQL snippets evaluation in `org-mode'
;; in dedicated buffer.
(setq ejc-org-mode-show-results nil)
(setq ejc-use-flx t)                          ; Enable `flx' fuzzy matching.
(setq ejc-result-table-impl 'ejc-result-mode) ; Set major-mode for results.

(eval-after-load 'company
  (add-hook 'ejc-sql-mode-hook
          (lambda () (setq-local company-backends
				 (cl-adjoin '(ejc-company-backend :with company-yasnippet) company-backends :test #'equal)))))

(add-hook 'sql-mode-hook
	  (lambda ()
	    (ejc-sql-mode t)))

(add-hook 'ejc-sql-minor-mode-hook
	  (lambda ()
	    (ejc-eldoc-setup)))

(setq ejc-completion-system 'standard)

;; pip3 install shandy-sqlfmt
(setq sqlformat-command 'sqlformat)
(define-key sql-mode-map (kbd "C-c C-f") 'sqlfmt)


(provide 'init-ejc)

;;; init-ejc.el ends here
