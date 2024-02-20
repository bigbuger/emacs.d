;;; init-glpk.el --- glpk/gmpl setting. glpk is a tool for solve liner problems.
;;; Commentary:
;; 

;;; Code:

(use-package gmpl-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.mod\\'" . gmpl-mode))

  :config
  (with-eval-after-load 'company-keywords
    (add-to-list 'company-keywords-alist
		 '(gmpl-mode "and" "else" "mod" "union"
			     "by" "if" "not" "within"
			     "cross" "in" "or"
			     "diff" "inter" "symdiff"
			     "div" "less" "then"
			     "maximize" "minimize"
			     "dimen" "default" "integer" "binary" "symbolic"
			     "for" "check" "table" "IN" "OUT"
			     "data" "end" "solve"
			     "s.t." "subject to" "subj to"
			     "sum" "prod" "min" "max" "setof" "forall" "exists"
			     "abs" "atan" "card" "ceil" "cos"
			     "exp" "floor" "gmtime" "length"
			     "log" "log10" "max" "min" "round"
			     "sin" "sqrt" "str2time" "trunc"
			     "Irand224" "Uniform01" "Uniform"
			     "Normal01" "Normal"
			     "substr" "time2str"
			     "printf")))
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
(provide 'ob-gmpl)

(provide 'init-glpk)

;;; init-glpk.el ends here
