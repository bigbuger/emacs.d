;;; init-glpk.el --- glpk/gmpl setting. glpk is a tool for solve liner problems.
;;; Commentary:
;; 

;;; Code:

(use-package gmpl-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.mod\\'" . gmpl-mode))
  (with-eval-after-load 'flycheck
    (flycheck-define-checker gmpl
      "A gplk/gmpl syntax checker."
      :command ("glpsol" "--check" "-m" source)
      :error-filter (lambda (errors)
		      (seq-filter (lambda (err)
				    ;; (message (flycheck-error-message err))
				    (not (string-prefix-p "no value for"
							  (string-trim (flycheck-error-message err)))))
				  errors))
      :error-patterns
      (
       (warning line-start (file-name) ":" line ": warning:" (message)  line-end)
       (error line-start (file-name) ":" line ":" (message)  line-end))
      :modes gmpl-mode)
    (add-to-list 'flycheck-checkers 'gmpl))

  :config
  (with-eval-after-load 'company-keywords
    (add-to-list 'company-keywords-alist
		 '(gmpl-mode "set" "param" "var"
			     "maximize" "minimize"
			     "s.t." "subject to" "subj to"
			     
			     "and" "else" "mod" "union"
			     "by" "if" "not" "within"
			     "cross" "in" "or"
			     "diff" "inter" "symdiff"
			     "div" "less" "then"
			     
			     "dimen" "default" "integer" "binary" "symbolic"
			     "for" "check" "table" "IN" "OUT"
			     
			     "data" "end" "solve"
			     
			     "sum" "prod" "min" "max" "setof" "forall" "exists"

			     "abs" "atan" "card" "ceil" "cos"
			     "exp" "floor" "gmtime" "length"
			     "log" "log10" "max" "min" "round"
			     "sin" "sqrt" "str2time" "trunc"
			     "Irand224" "Uniform01" "Uniform"
			     "Normal01" "Normal"
			     
			     "substr" "time2str"
			     
			     "display" "printf")))
  )

(defun org-babel-execute:gmpl (body params)
  "Execute a block of gmpl(glpk) code with org-babel."
  (let ((in-file (org-babel-temp-file "gmpl" ".mod"))
	(cmdline (or (cdr (assq :cmdline params)) "")))
    (with-temp-file in-file
      (insert body))
    (org-babel-eval
     (format "glpsol -m %s %s" (org-babel-process-file-name in-file) cmdline)
     "")))

(with-eval-after-load 'org
    (add-to-list 'org-babel-load-languages
		 '(gmpl . t)))

(provide 'ob-gmpl)

(provide 'init-glpk)

;;; init-glpk.el ends here
