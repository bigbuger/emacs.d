;;; init-json.el --- init json mode config


;;; Commentary:
;; 

;;; Code:
(require 'json-mode)

(add-hook 'json-mode-hook 'tree-sitter-mode)
(add-hook 'json-mode-hook 'ts-fold-indicators-mode)

(add-to-list 'load-path "~/.emacs.d/lisp/libs/counsel-jq-ex/")
(require 'counsel-jq-ex)

(define-key json-mode-map (kbd "C-c C-j") 'counsel-jq-ex)
(define-key js-mode-map (kbd "C-c C-j") 'counsel-jq-ex)


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

(provide 'init-json)

;;; init-json.el ends here
