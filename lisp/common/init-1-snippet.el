;;; init-snippet.el --- 文字模版相关工具
;;yasnippet

;;; Commentary:
;; 

;;; Code:

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(require 'yasnippet)
(yas-global-mode 1)
(add-hook 'minibuffer-setup-hook 'yas-minor-mode)


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

;; copy from https://www.emacswiki.org/emacs/HippieExpand
(defun try-expand-flexible-abbrev (old)
  "Try to complete word using flexible matching.

Flexible matching works by taking the search string and then
interspersing it with a regexp for any character. So, if you try
to do a flexible match for `foo' it will match the word
`findOtherOtter' but also `fixTheBoringOrange' and
`ifthisisboringstopreadingnow'.

The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (if (not old)
      (progn
	(he-init-string (he-lisp-symbol-beg) (point))
	(if (not (he-string-member he-search-string he-tried-table))
	    (setq he-tried-table (cons he-search-string he-tried-table)))
	(setq he-expand-list
	      (and (not (equal he-search-string ""))
		   (he-flexible-abbrev-collect he-search-string)))))
  (while (and he-expand-list
	      (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
	(if old (he-reset-string))
	())
      (progn
	(he-substitute-string (car he-expand-list))
	(setq he-expand-list (cdr he-expand-list))
	t)))

(defun he-flexible-abbrev-collect (str)
  "Find and collect all words that flex-matches STR.
See docstring for `try-expand-flexible-abbrev' for information
about what flexible matching means in this context."
  (let ((collection nil)
        (regexp (he-flexible-abbrev-create-regexp str)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp regexp nil t)
        ;; Is there a better or quicker way than using
        ;; `thing-at-point' here?
        (setq collection (cons (thing-at-point 'word) collection))))
    collection))

(defun he-flexible-abbrev-create-regexp (str)
  "Generate regexp for flexible matching of STR.
See docstring for `try-expand-flexible-abbrev' for information
about what flexible matching means in this context."
  (concat "\\b" (mapconcat (lambda (x) (concat "\\w*" (list x))) str "")
          "\\w*" "\\b"))

(defun hippie-expand-flex ()
  "Call try-expand-flexible-abbrev."
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-expand-flexible-abbrev)))
    (hippie-expand nil)))

(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
	
	try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
	
	my/try-expand-list
        try-expand-list
	try-expand-line 		;It is `C-u 7 M-/'
	try-expand-list-all-buffers
	try-expand-line-all-buffers
	))

(require 'bind-key)
(bind-key* "M-h" 'hippie-expand) ;; 原来是 mark-paragraph, 不过我不怎么用

(defun hippie-expand-line ()
  "Call try-expand-line."
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-expand-line try-expand-line-all-buffers)))
    (hippie-expand nil)))
(global-set-key (kbd "M-l") 'hippie-expand-line) ; 原来是 downcase-word, 不过我不怎么用

(defun he-fix-string-parens (args)
  "remove extra paren when expanding line in smartparens."
  (let* ((str (car args))
	 (last (substring str -1)))
    (if (and smartparens-mode
	     (member last '(")" "}" "\"" "'" "`" "]"))
	     (string= last (char-to-string (char-after))))
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
      (ignore-errors
	(let ((old-point (point))
              (old-tick (buffer-chars-modified-tick)))
          (ignore-errors (yas-next-field))
          (when (and (eq old-point (point))
                     (eq old-tick (buffer-chars-modified-tick)))
            (company-complete-common)))))
  (company-complete-common))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "<tab>") #'smarter-yas-expand-next-field-complete))

(add-to-list 'load-path "~/.emacs.d/lisp/libs/flash-fill.el")
(require 'flash-fill)
(global-set-key (kbd "M-L") #'flash-fill-line)

(provide 'init-1-snippet)

;;; init-1-snippet.el ends here
