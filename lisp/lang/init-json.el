;;; init-json.el --- init json mode config


;;; Commentary:
;; 

;;; Code:
(require 'json-mode)
(add-to-list 'treesit-language-source-alist
	     '(json "https://github.com/tree-sitter/tree-sitter-json"))

(use-package consult-jq
  :load-path "~/.emacs.d/lisp/libs/consult-jq/"
  :config
  (setq consult-jq-completion-styles '(basic partial-completion))
  :bind (:map json-mode-map
	      ("C-c C-j" . 'consult-jq)
	      ("C-c i"   . 'consult-jq)))

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq tab-width 4)
            (setq js-indent-level 4)))

(defun json-sort-keys ()
  "Sort json by keys using jq."
  (interactive)
  (let* ((cmd "jq --sort-keys --indent 4")
	 (test-cmd "jq . 1>>/dev/null")
	 (test-run (shell-command-on-region (point-min) (point-max) test-cmd nil nil "*jq error log*" t)))
    (if (= test-run 0)
	(save-excursion
	  (shell-command-on-region (point-min) (point-max) cmd  (current-buffer) 1)))))

(defun json-format-by-jq ()
  "Format json by keys using jq."
  (interactive)
  (let* ((cmd "jq --indent 4 .")
	 (test-cmd "jq . 1>>/dev/null")
	 (test-run (shell-command-on-region (point-min) (point-max) test-cmd nil nil "*jq error log*" t)))
    (if (= test-run 0)
	(save-excursion
	  (shell-command-on-region (point-min) (point-max) cmd  (current-buffer) 1)))))

(define-key json-mode-map (kbd "C-c C-s") 'json-sort-keys)

(if (executable-find "jq")
    (define-key json-mode-map (kbd "C-c C-f") 'json-format-by-jq))

(use-package jq-mode)

(provide 'init-json)

;;; init-json.el ends here
