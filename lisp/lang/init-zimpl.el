;;; init-zimpl.el --- Zimpl is a little language to translate the mathematical model of a problem into a linear or nonlinear (mixed-) integer mathematical program expressed in .lp or .mps file format which can be read and (hopefully) solved by a LP or MIP solver.




;;; Commentary:
;; 

;;; Code:

(use-package zimpl-mode
  :load-path "~/.emacs.d/lisp/libs/zimpl-mode"

  :config
  (with-eval-after-load 'company-keywords
    (add-to-list 'company-keywords-alist
		 '(zimpl-mode
		   "abs" "and" "argmax" "argmin" "as" "binary" "by" "card"
		   "ceil" "check" "checkonly" "comment" "cos" "cross"
		   "default" "defbool" "defnumb" "defset" "defstrg" "div"
		   "do" "else" "end" "exists" "exp" "floor" "forall" "if"
		   "implicit" "in" "indexset" "indicator" "infinity"
		   "integer" "inter" "length" "ln" "log" "match" "max" "min"
		   "mod" "modulo" "not" "or" "ord" "pow" "powerset" "print"
		   "priority" "prod" "proj" "random" "read" "real" "round"
		   "scale" "separate" "sgn" "sgnpow" "sin" "skip" "sos"
		   "sqrt" "startval" "subsets" "substr" "sum" "symdiff"
		   "tan" "then" "to" "type1" "type2" "union" "use" "vabs"
		   "vif" "with" "without" "xor"
		   "maximize" "minimize" "param" "set" "subto" "var"
		   "from" "import" "as" "class"
		   "self" "None" "True" "False" "Ellipsis")))
  )



(provide 'init-zimpl)

;;; init-zimpl.el ends here
