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

(require 'hippie-exp)

(defun my/he-list-beg ()
    (save-excursion
    (condition-case ()
	(progn
	  (backward-up-list 1)
	  (forward-char 1))
      (error ()))
    (point)))

(defun my/try-expand-list (old)
  "Try to complete the current beginning of a list.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (let ((expansion ()))
    (if (not old)
	(progn
	  (he-init-string (my/he-list-beg) (point))
	  (set-marker he-search-loc he-string-beg)
	  (setq he-search-bw t)))

    (if (not (equal he-search-string ""))
	(save-excursion
	  (save-restriction
	    (if hippie-expand-no-restriction
		(widen))
	    ;; Try looking backward unless inhibited.
	    (if he-search-bw
		(progn
		  (goto-char he-search-loc)
		  (setq expansion (he-list-search he-search-string t))
		  (set-marker he-search-loc (point))
		  (if (not expansion)
		      (progn
			(set-marker he-search-loc he-string-end)
			(setq he-search-bw ())))))

	    (if (not expansion) ; Then look forward.
		(progn
		  (goto-char he-search-loc)
		  (setq expansion (he-list-search he-search-string nil))
		  (set-marker he-search-loc (point)))))))

    (if (not expansion)
	(progn
	  (if old (he-reset-string))
	  ())
	(progn
	  (he-substitute-string expansion t)
	  t))))


(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
	my/try-expand-list
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

(defun smarter-yas-expand-next-field-complete ()
    "Try to `yas-expand' and `yas-next-field' at current cursor position.

If failed try to complete the common part with `company-complete-common'"
    (interactive)
    (if yas-minor-mode
        (let ((old-point (point))
              (old-tick (buffer-chars-modified-tick)))
          (ignore-errors (yas-next-field))
          (when (and (eq old-point (point))
                     (eq old-tick (buffer-chars-modified-tick)))
            (company-complete-common))))
    (company-complete-common))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "<tab>") #'smarter-yas-expand-next-field-complete))

(provide 'init-1-snippet)

;;; init-1-snippet.el ends here
