;;; my-ejc.el --- emacs ejc-sql config

;;; Commentary:
;; 

(require 'ejc-sql)
(require 'ejc-company)
(require 'ejc-eldoc)

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
  (lambda ()
    (push 'ejc-company-backend company-backends)))

(add-hook 'sql-mode-hook
	  (lambda ()
	    (ejc-sql-mode t)))

(add-hook 'ejc-sql-minor-mode-hook
	  (lambda ()
	    (ejc-eldoc-setup)))

(provide 'my-ejc)

;;; my-ejc.el ends here
