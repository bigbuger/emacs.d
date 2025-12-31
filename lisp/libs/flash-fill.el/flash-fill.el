;;; flash-fill.el --- Automating String Processing. Like excel's `C-e'. -*- lexical-binding: t; -*-

;;; Commentary:
;;

(defcustom flash-fill-processor-list
  '(flash-fill-make-processor-just-by-find-colmun
    flash-fill-identity)
  "Flash fill processor list.")

(defconst flash-fill-column-regxp
  (rx (group symbol-start (+ word) symbol-end) (group (* (not word)))))

(defun flash-fill-make-processor-just-by-find-colmun (target inputs)
  (let ((context (car target))
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
	(format "%s%s"(car (aref inputs match-pos))
		(cdr speartor))))))

(defun flash-fill-identity (target _inputs)
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
	(inputs ))))

(defun flash-fill-line ()
  (interactive)
  (let* ((example-columns (save-excursion (forward-line -1) (flash-fill-collect-line-columns)))
	 (current-columns (flash-fill-collect-line-columns))
	 (fill-start (length example-columns))
	 (fill-processor-list (flash-fill-make-processor fill-start example-columns))
	 (fill-result (concat (mapcar (lambda (f) (funcall f current-columns)) fill-processor-list))))
    (end-of-line)
    (insert fill-result)))

(provide 'flash-fill)

;;; flash-fill.el ends here
