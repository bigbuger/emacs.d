;;; init-json.el --- init json mode config


;;; Commentary:
;; 

;;; Code:
(setq js-indent-level 4)
(setq json-encoding-default-indentation (make-string js-indent-level ?\s))
(require 'json-mode)
(add-to-list 'treesit-language-source-alist
	     '(json "https://github.com/tree-sitter/tree-sitter-json"))

(use-package consult-jq
  :load-path "~/.emacs.d/lisp/libs/consult-jq/"
  :config
  (setq consult-jq-completion-styles '(basic partial-completion emacs22 substring))
  :bind (:map json-mode-map
	      ("C-c C-j" . 'consult-jq)
	      ("C-c i"   . 'consult-jq)))

(add-hook 'json-mode-hook
          (lambda ()
            (setq-local tab-width js-indent-level)
	    (when (executable-find "jq")
	      (setq-local flycheck-checker 'json-jq))))

(defun json-sort-keys ()
  "Sort json by keys using jq."
  (interactive)
  (let* ((cmd (format "jq --sort-keys --indent %d" js-indent-level))
	 (test-cmd "jq . 1>>/dev/null")
	 (test-run (shell-command-on-region (point-min) (point-max) test-cmd nil nil "*jq error log*" t)))
    (if (= test-run 0)
	(save-excursion
	  (shell-command-on-region (point-min) (point-max) cmd  (current-buffer) 1)))))

(define-key json-mode-map (kbd "C-c C-s") 'json-sort-keys)

(use-package format-all
  :config
  ;; gof is a shell file, it run gofmt firset, goimports second
  (define-format-all-formatter jq
    (:executable "jq")
    (:install)
    (:languages "JSON")
    (:features)
    (:format (format-all--buffer-easy executable "--indent" (number-to-string js-indent-level))))

  :hook
  (json-mode . (lambda ()
		  (setq-local format-all-formatters
			      '(("JSON" jq))))))


(use-package jq-mode)

(provide 'init-json)

;;; init-json.el ends here
