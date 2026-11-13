;;; mlr-mode.el --- Edit Miller dsl scripts

;; Author: bigbuger
;; Package-Requires:

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Mode for editing Miller dsl scripts.

;;; Code:

(defgroup mlr nil
  "Major mode fro editing miller dsl."
  :group 'languages)

(defcustom mlr-indent-offset 2
  "*Indentation offset for `mlr-mode'."
  :group 'mlr
  :type 'integer)

(defvar mlr-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; Strings
    (modify-syntax-entry ?\" "\"\"" syntax-table)
    ;; Symbols
    (modify-syntax-entry ?_ "w" syntax-table)
    (modify-syntax-entry ?$ "_" syntax-table)
    (modify-syntax-entry ?@ "_" syntax-table)
    ;; Comments
    (modify-syntax-entry ?# "<" syntax-table)
    (modify-syntax-entry ?\n ">" syntax-table)
    (modify-syntax-entry ?\" "\"" syntax-table)

    ;; Parenthesis
    (modify-syntax-entry ?\( "()" syntax-table)
    (modify-syntax-entry ?\) ")(" syntax-table)
    (modify-syntax-entry ?\{ "(}" syntax-table)
    (modify-syntax-entry ?\} "){" syntax-table)
    (modify-syntax-entry ?\[ "(]" syntax-table)
    (modify-syntax-entry ?\] ")[" syntax-table)

    ;; Operators
    (modify-syntax-entry ?+ "." syntax-table)
    (modify-syntax-entry ?- "." syntax-table)
    (modify-syntax-entry ?* "." syntax-table)
    (modify-syntax-entry ?/ "." syntax-table)
    (modify-syntax-entry ?% "." syntax-table)
    (modify-syntax-entry ?^ "." syntax-table)
    (modify-syntax-entry ?= "." syntax-table)
    (modify-syntax-entry ?> "." syntax-table)
    (modify-syntax-entry ?< "." syntax-table)
    (modify-syntax-entry ?| "." syntax-table)
    (modify-syntax-entry ?& "." syntax-table)
    (modify-syntax-entry ?: "." syntax-table)
    (modify-syntax-entry ?~ "." syntax-table)
    (modify-syntax-entry ?! "." syntax-table)
    syntax-table)
  "Syntax table for `mlr-mode'.")

(defconst mlr--keywords
  '("begin" "do" "elif" "else" "end"
    "filter" "for" "if" "in"
    "while" "break" "continue" "return"
    "func" "subr" "call" "dump" "edump"
    "emit" "emitp" "emitf"
    "eprint" "eprintn" "print" "printn"
    "tee" "stdout" "stderr" "unset" "null"
    )
  "The keywords used in mlr.")

(defconst mlr--constants
  '("true" "false" "M_PI" "M_E")
  "The Constants in mlr.")

(defconst mlr--context-variables
  '("IPS" "IFS" "IRS" "OPS" "OFS"
    "ORS" "OFLATSEP" "NF" "NR" "FNR"
    "FILENAME" "FILENUM" "ENV")
  "The context variables in mlr.")

(defconst mlr--types
  '("arr" "bool" "float" "int" "map"
    "num" "str" "var")
  "The types in mlr.")

(defconst mlr--builtins
  '(;;Arithmetic functions
    "bitcount" "madd" "mexp" "mmul" "msub"
    "pow"
    ;;"%" "&" "*" "**" "+" "-" ".*" ".+" ".-" "./" "/" "//" "<<" ">>" ">>>" "^" "|" "~"

    ;; Boolean functions
    ;;"!" "!=" "!=~" "&&" "<" "<=" "<=>" "==" "=~" ">" ">=" "?" "??" "???" "^^" "||"

    ;;Collections functions
    "append" "arrayify" "concat" "depth" "flatten"
    "get_keys" "get_values" "haskey" "hasvalue" "json_parse"
    "json_stringify" "leafcount" "length" "mapdiff" "mapexcept"
    "mapselect" "mapsum" "unflatten"

    ;;Conversion functions
    "boolean" "bytes" "float" "fmtifnum" "fmtnum"
    "hexfmt" "int" "joink" "joinkv" "joinv" "splita"
    "splitax" "splitkv" "splitkvx" "splitnv" "splitnvx"
    "string"

    ;;Hashing functions
    "md5" "sha1" "sha256" "sha512"

    ;;Higher-order-functions functions
    "any" "apply" "every" "fold" "reduce"
    "select" "sort"

    ;;Math functions
    "abs" "acos" "acosh" "asin" "asinh"
    "atan" "atan2" "atanh" "cbrt" "ceil"
    "cos" "cosh" "erf" "erfc" "exp"
    "expm1" "floor" "invqnorm" "log" "log10"
    "log1p" "logifit" "max" "min" "qnorm"
    "round" "roundm" "sgn" "sin" "sinh"
    "sqrt" "tan" "tanh" "urand" "urand32"
    "urandelement" "urandint" "urandrange"

    ;;Stats functions
    "antimode" "count" "distinct_count" "kurtosis" "maxlen"
    "mean" "meaneb" "median" "minlen" "mode"
    "null_count" "percentile" "percentiles" "skewness" "sort_collection"
    "sparkline" "stddev" "sum" "sum2" "sum3"
    "sum4" "variance"

    ;;String functions
    "base64_decode" "base64_encode" "capitalize" "clean_whitespace" "collapse_whitespace"
    "contains" "format" "gssub" "gsub" "hex_decode"
    "hex_encode" "index" "latin1_to_utf8" "leftpad" "lstrip"
    "regextract" "regextract_or_else" "rightpad" "rstrip" "ssub"
    "strip" "strlen" "strmatch" "strmatchx" "sub"
    "substr" "substr0" "substr1" "tolower" "toupper"
    "truncate" "unformat" "unformatx" "utf8_to_latin1" ;;"."

    ;;System functions
    "exec" "hostname" "next" "os" "stat"
    "system" "version"

    ;;Time functions
    "datediff" "dhms2fsec" "dhms2sec" "fsec2dhms" "fsec2hms"
    "gmt2localtime" "gmt2nsec" "gmt2sec" "hms2fsec" "hms2sec"
    "localtime2gmt" "localtime2nsec" "localtime2sec" "nsec2gmt" "nsec2gmtdate"
    "nsec2localdate" "nsec2localtime" "sec2dhms" "sec2gmt" "sec2gmtdate"
    "sec2hms" "sec2localdate" "sec2localtime" "strfntime" "strfntime_local"
    "strftime" "strftime_local" "strpntime" "strpntime_local" "strptime"
    "strptime_local" "sysntime" "systime" "systimeint" "upntime" "uptime"

    ;;Typing functions
    "asserting_absent" "asserting_array" "asserting_bool" "asserting_boolean" "asserting_bytes"
    "asserting_empty" "asserting_empty_map" "asserting_error" "asserting_float" "asserting_int"
    "asserting_map" "asserting_nonempty_map" "asserting_not_array" "asserting_not_empty" "asserting_not_map"
    "asserting_not_null" "asserting_null" "asserting_numeric" "asserting_present" "asserting_string"
    "is_absent" "is_array" "is_bool" "is_boolean" "is_bytes"
    "is_empty" "is_empty_map" "is_error" "is_float" "is_int"
    "is_map" "is_nan" "is_nonempty_map" "is_not_array" "is_not_empty"
    "is_not_map" "is_not_null" "is_null" "is_numeric" "is_present"
    "is_string" "typeof")

  "The builtin function in mlr.
See https://miller.readthedocs.io/en/latest/reference-dsl-builtin-functions/#functions-by-class")

(defconst mlr--capf-canditians
  (append mlr--keywords
	  mlr--builtins
	  mlr--types
	  mlr--constants
	  mlr--context-variables)
  "Canditians using for capf in `mlr-mode'")

(defconst mlr-defun-regex "\\_<\\(?:func\\|subr\\)\\s-+\\([_[:alpha:]][_[:alnum:]]*\\)[^{]*")

(defvar mlr-font-lock-keywords
  `(;; Variables
    ("\\_<\\$\\w+" 0 font-lock-variable-name-face)
    ("\\_<@\\w+" 0 font-lock-variable-name-face)

    (,(regexp-opt mlr--keywords 'symbols) . font-lock-keyword-face)
    (,(regexp-opt mlr--types 'symbols) . font-lock-type-face)
    (,(regexp-opt mlr--builtins 'symbols) . font-lock-builtin-face)
    (,(regexp-opt mlr--constants 'word) . font-lock-constant-face)
    (,(regexp-opt mlr--context-variables 'word) . font-lock-keyword-face)

    (,mlr-defun-regex (1 font-lock-function-name-face))
    ))

(defun mlr-completion-at-point ()
  "The capf or mlr-mode."
  (when-let* ((bnds (bounds-of-thing-at-point 'symbol)))
    (unless (eq ?$ (char-before (car bnds))) ; Ignore variables
      (list (car bnds) (cdr bnds) mlr--capf-canditians))))

(defvar company-keywords-alist)
(with-eval-after-load 'company-keywords
  (add-to-list 'company-keywords-alist
	       `(mlr-mode . ,mlr--capf-canditians)))

(defvar mlr-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `mlr-mode'.")

(defun mlr-indent-line ()
  "Indent current line."
  (let (indent
        boi-p      ;begin of indent
        move-eol-p
        (current (point)))
    (save-excursion
      (back-to-indentation)
      (setq indent (car (syntax-ppss))
            boi-p (= current (point)))
      ;; don't indent empty lines if they don't have the in it
      (when (and (eq (char-after) ?\n)
                 (not boi-p))
        (setq indent 0))
      ;; check whether we want to move to the end of line
      (when boi-p
        (setq move-eol-p t))
      ;; decrement the indent if the first character on the line is a
      ;; closer.
      (when (or (eq (char-after) ?\))
                (eq (char-after) ?\}))
        (setq indent (1- indent)))
      ;; indent the line
      (delete-region (line-beginning-position)
                     (point))
      (indent-to (* mlr-indent-offset indent)))
    (when move-eol-p
      (move-end-of-line nil))))

;;;###autoload
(define-derived-mode mlr-mode prog-mode "Miller"
  "Major mode for miller dsl.
\\{mlr-mode-map}"
  :group 'mlr
  (setq-local font-lock-defaults '(mlr-font-lock-keywords))
  (setq-local comment-start "# ")
  (setq-local indent-line-function #'mlr-indent-line)
  (setq-local defun-prompt-regexp mlr-defun-regex) ;; TODO what if have a `{' inside comment?
  (setq-local imenu-generic-expression `(("*Functions*" ,mlr-defun-regex 1)))
  (add-hook 'completion-at-point-functions #'mlr-completion-at-point nil t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mlr\\'" . mlr-mode))


(provide 'mlr-mode)

;;; mlr-mode.el ends here
