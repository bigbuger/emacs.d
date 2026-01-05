;;; flash-fill.el --- Automating String Processing. Like excel's `C-e'. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;; Code:

(require 'cl-lib)
(require 's)

(defgroup flash-fill nil
  "Flash fill, automating String Processing.")

(defcustom flash-fill-processor-maker-list
  '(flash-fill-make-processor-find-column
    flash-fill-make-processor-find-column-capitalize
    flash-fill-make-processor-find-column-downcase
    flash-fill-make-processor-find-column-upcase
    flash-fill-make-processor-find-column-camel
    flash-fill-make-processor-find-column-upper-camel
    flash-fill-make-processor-find-column-snake
    flash-fill-make-processor-find-column-upper-snake)
  "Flash fill processor maker list."
  :group 'flash-fill
  :local t
  :type '(repeat (choice (function :tag "Processor maker function")
			 ((list function function) "Condition function and Processor maker function"))))

(defcustom flash-fill-column-regxp
  (rx (group symbol-start (+ (or word "_")) symbol-end) (? ( group (* (not word)))))
  "Flash fill regex to collection the columns."
  :group 'flash-fill
  :local t
  :type '(regexp))

(defun flash-fill-make-processor-find-column-with-convert (target inputs convert)
  (let ((context (funcall convert (car target)))
	(speartor (cadr target))
	(iter 0)
	(match?)
	(match-idx))
    (while (and (< iter (length inputs))
		(not match?))
      (if (string-equal context (car (aref inputs iter)))
	  (progn (setq match? t)
		 (setq match-idx iter))
	(setq iter (+ 1 iter))))
    (when match?
     `(flash-fill--by-column-with-convert ,match-idx ,speartor ,convert))))

(defun flash-fill--by-column-with-convert (inputs match-idx speartor convert)
  "Fill by using `INPUTS' ref of `MATCH-IDX', after do `CONVERT'.
Concat with `SPEARTOR'"
  (format "%s%s" (funcall convert (car (aref inputs match-idx)))
	  speartor))

(defun flash-fill-make-processor-find-column (target inputs)
  (flash-fill-make-processor-find-column-with-convert target inputs #'identity))

(defun flash-fill-make-processor-find-column-capitalize (target inputs)
  (flash-fill-make-processor-find-column-with-convert target inputs #'capitalize))

(defun flash-fill-make-processor-find-column-downcase (target inputs)
  (flash-fill-make-processor-find-column-with-convert target inputs #'downcase))

(defun flash-fill-make-processor-find-column-upcase (target inputs)
  (flash-fill-make-processor-find-column-with-convert target inputs #'upcase))

(defun flash-fill-make-processor-find-column-camel (target inputs)
  (flash-fill-make-processor-find-column-with-convert target inputs #'s-lower-camel-case))

(defun flash-fill-make-processor-find-column-upper-camel (target inputs)
  (flash-fill-make-processor-find-column-with-convert target inputs #'s-upper-camel-case))

(defun flash-fill-make-processor-find-column-snake (target inputs)
  (flash-fill-make-processor-find-column-with-convert target inputs #'s-snake-case))

(defun flash-fill-make-processor-find-column-upper-snake (target inputs)
  (flash-fill-make-processor-find-column-with-convert target inputs
						      #'(lambda (str)
							  (upcase (s-snake-case str)))))

(defun flash-fill-identity (_inputs target)
  (string-join target))

(defun flash-fill-collect-line-columns ()
  "Collect all line column and speartor as a vector."
  (let ((columns []))
    (save-excursion
      (beginning-of-line)
      (while (re-search-forward flash-fill-column-regxp (line-end-position) t)
	(setq columns (vconcat columns
			       (make-vector 1 (list (match-string-no-properties 1)
						    (match-string-no-properties 2))))))
      columns)))

(defun flash-fill-make-processor (fill-start example-columns)
  (let ((targets (seq-subseq example-columns fill-start))
	(inputs (seq-subseq example-columns 0 fill-start)))
    (mapcar (lambda (target)
	      (let (processor)
		(cl-loop for m in flash-fill-processor-maker-list
			 until processor
			 do
			 (let ((tmp (funcall m target inputs)))
			   (when tmp
			     (setq processor tmp))))
		(or processor `(flash-fill-identity ,target))))
	    targets)))

;; TODO multiple processor from more then one example
(defun flash-fill-make-multi-processor (fill-start example-columns)
  (let ((targets (seq-subseq example-columns fill-start))
	(inputs (seq-subseq example-columns 0 fill-start)))
    (mapcar (lambda (target)
	      (let ((processors
		     (append (mapcard
			      (lambda (m) (funcall m target inputs)
				flash-fill-processor-maker-list))
			     (flash-fill-identity target))))
		(cl-remove-if-not #'identity processors)))
	    targets)))

(defun flash-fill-line ()
  (interactive)
  (let* ((example-columns (save-excursion (forward-line -1) (flash-fill-collect-line-columns)))
	 (current-columns (flash-fill-collect-line-columns))
	 (fill-start (length current-columns))
	 (fill-processor-list (flash-fill-make-processor fill-start example-columns))
	 (fill-result (string-join (mapcar (lambda (processor) (apply (car processor) current-columns (cdr processor))) fill-processor-list))))
    (end-of-line)
    (insert fill-result)))

(defun flash-fill-region ()
  (interactive)
  (when (region-active-p)
    (save-excursion
      (when (> (region-beginning) (region-end))
	(exchange-point-and-mark))
      (let ((start-point (region-beginning))
	    (end-line (line-number-at-pos (region-end))))
	(goto-char (region-end))
	(if (= (current-column) 0)
	    (setq end-line (- end-line 1)))
	(goto-char start-point)
	(let* ((example-columns (flash-fill-collect-line-columns)))
	  (while (< (line-number-at-pos) end-line)
	    (beginning-of-line)
	    (forward-line)
	    (let* ((current-columns (flash-fill-collect-line-columns))
		   (fill-start (length current-columns))
		   (fill-processor-list (flash-fill-make-processor fill-start example-columns)) ; TODO do not re calc when the columns length is same for previous line
		   (fill-result (string-join (mapcar (lambda (processor) (apply (car processor) current-columns (cdr processor))) fill-processor-list))))
	      (end-of-line)
	      (insert fill-result))))))))

(defun flash-fill-region-or-line ()
  (interactive)
  (if (region-active-p)
      (flash-fill-region)
    (flash-fill-line)))

(provide 'flash-fill)

;;; flash-fill.el ends here
