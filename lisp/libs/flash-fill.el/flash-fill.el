;;; flash-fill.el --- Automating String Processing. Like excel's `C-e'. -*- lexical-binding: t; -*-

;;; Commentary:
;;

(require 'cl-lib)
(require 's)

(defcustom flash-fill-processor-list
  '(flash-fill-make-processor-find-column
    flash-fill-make-processor-find-column-capitalize
    flash-fill-make-processor-find-column-downcase
    flash-fill-make-processor-find-column-upcase
    flash-fill-make-processor-find-column-camel
    flash-fill-make-processor-find-column-upper-camel
    flash-fill-make-processor-find-column-snake
    flash-fill-make-processor-find-column-upper-snake
    flash-fill-identity)
  "Flash fill processor list.")

(defvar-local flash-fill-column-regxp
  (rx (group symbol-start (+ (or word "_")) symbol-end) (? ( group (* (not word))))))

(defun flash-fill-make-processor-find-column-with-convert (target inputs convert)
  (let ((context (funcall convert (car target)))
	(speartor (cadr target))
	(iter 0)
	(match?)
	(match-pos))
    (while (and (< iter (length inputs))
		(not match?))
      (if (string-equal context (car (aref inputs iter)))
	  (progn (setq match? t)
		 (setq match-pos iter))
	(setq iter (+ 1 iter))))
    (when match?
      (lambda (inputs)
	(format "%s%s"
		(funcall convert (car (aref inputs match-pos)))
		speartor)))))

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
							(upcase (s-snake-case srt)))))

(defun flash-fill-identity (target _inputs)
  (lambda (_)
    (string-join target)))

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
		(cl-loop for m in flash-fill-processor-list
			 until processor
			 do
			 (let ((tmp (funcall m target inputs)))
			   (when tmp
			     (setq processor tmp))))
		(or processor #'flash-fill-identity)))
	    targets)))

(defun flash-fill-line ()
  (interactive)
  (let* ((example-columns (save-excursion (forward-line -1) (flash-fill-collect-line-columns)))
	 (current-columns (flash-fill-collect-line-columns))
	 (fill-start (length current-columns))
	 (fill-processor-list (flash-fill-make-processor fill-start example-columns))
	 (fill-result (string-join (mapcar (lambda (f) (funcall f current-columns)) fill-processor-list))))
    (end-of-line)
    (insert fill-result)))

(defun flash-fill-region ()
  (interactive)
  (when (region-active-p)
    (let ((start-point (region-beginning))
	  (end-line (line-number-at-pos (region-end))))
      (save-excursion
	(goto-char start-point)
	(let* ((example-columns (flash-fill-collect-line-columns)))
	  (while (< (line-number-at-pos) end-line)
	    (beginning-of-line)
	    (forward-line)
	    (let* ((current-columns (flash-fill-collect-line-columns))
		   (fill-start (length current-columns))
		   (fill-processor-list (flash-fill-make-processor fill-start example-columns)) ; TODO do not re calc when the columns length is same for previous line
		   (fill-result (string-join (mapcar (lambda (f) (funcall f current-columns)) fill-processor-list))))
	      (end-of-line)
	      (insert fill-result))))))))

(provide 'flash-fill)

;;; flash-fill.el ends here
