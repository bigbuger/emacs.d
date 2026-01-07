;;; flash-fill.el --- Automating String Processing. Like excel's `C-e'. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;; Code:

(require 'cl-lib)
(require 's)

(defgroup flash-fill nil
  "Flash fill, automating String Processing."
  :prefix "flash-fill-"
  :group 'flash-fill)

(defcustom flash-fill-processor-maker-list
  '(flash-fill-make-processor-find-column
    flash-fill-make-processor-find-column-capitalize
    flash-fill-make-processor-find-column-downcase
    flash-fill-make-processor-find-column-upcase
    flash-fill-make-processor-find-column-camel
    flash-fill-make-processor-find-column-upper-camel
    flash-fill-make-processor-find-column-snake
    flash-fill-make-processor-find-column-upper-snake
    flash-fill-make-processor-find-substring)
  "Flash fill processor maker list."
  :group 'flash-fill
  :local t
  :type '(repeat (function :tag "Processor maker functions")))

(defcustom flash-fill-column-regxp
  (rx (group word-start (+ (or word "_")) word-end) (? ( group (* (not word)))))
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

(defcustom flash-fill-substring-mini-length
  5
  "Mini length of target that can use substring process."
  :group 'flash-fill
  :local t
  :type '(string))

(defun flash-fill--by-column-substring (inputs match-idx speartor substr-from length)
  "Fill by using `INPUTS' ref of `MATCH-IDX', after do `CONVERT'.
Concat with `SPEARTOR'"
  (or (ignore-errors
       (format "%s%s" (substring (car (aref inputs match-idx)) substr-from (+ substr-from length))
	       speartor)) ""))

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

(defun flash-fill-make-processor-find-substring (target inputs)
  (let ((context (car target))
	(speartor (cadr target))
	(iter 0)
	(match?)
	(match-idx)
	(substr-idx))
    (when (not (length< context flash-fill-substring-mini-length))
      (while (and (< iter (length inputs))
		  (not match?))
	(if-let* ((current-substr-idx (string-search context (car (aref inputs iter)))))
	    (progn (setq match? t)
		   (setq match-idx iter)
		   (setq substr-idx current-substr-idx))
	  (setq iter (+ 1 iter))))
      (when match?
	`(flash-fill--by-column-substring ,match-idx ,speartor ,substr-idx ,(length context))))))

(defun flash-fill-identity (_inputs target)
  "Just return `TARGET' as string."
  (string-join target))

(defun flash-fill-collect-line-columns ()
  "Collect all column and separator as a vector."
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


(defun flash-fill-make-processor-for-region (fill-start example-rows)
  (let* ((first-row (car example-rows))
	 (other-rows (cdr example-rows))
	 (first-inputs (seq-subseq first-row 0 fill-start))
	 (first-targets (seq-subseq first-row fill-start))
	 (other-inputs (mapcar (lambda (r) (seq-subseq r 0 fill-start)) other-rows))
	 (other-targets (mapcar (lambda (r) (seq-subseq r fill-start)) other-rows))
	 result)
    (if (not other-rows)
	(flash-fill-make-processor fill-start first-row)
      (dotimes (target-column-iter (length first-targets) result)
	(let* ((target (aref first-targets target-column-iter))
	       (processors-canditions
		(cl-remove-if-not #'identity
				  (mapcar (lambda (m) (funcall m target first-inputs)) flash-fill-processor-maker-list))))
	  (setq processors-canditions
		(cl-remove-if-not (lambda (processor)
				    (let ((match-all? t))
				      (dotimes (other-rows-iter (length other-inputs) match-all?)
					(let* ((inputs (nth other-rows-iter other-inputs))
					       (target (aref (nth other-rows-iter other-targets) target-column-iter))
					       (match (string-equal
						       (string-join target)
						       (ignore-errors (apply (car processor) inputs (cdr processor))))))
					  (setq match-all? (and match-all? match))))))
				  processors-canditions))
	  (setq result
		(append result (list (or (car processors-canditions) `(flash-fill-identity ,target))))))))))

;;;###autoload
(defun flash-fill-line ()
  "Automating fill current line base on previous line."
  (interactive)
  (let* ((example-columns (save-excursion (forward-line -1) (flash-fill-collect-line-columns)))
	 (current-columns (flash-fill-collect-line-columns))
	 (fill-start (length current-columns))
	 (fill-processor-list (flash-fill-make-processor fill-start example-columns))
	 (fill-result (string-join (mapcar (lambda (processor) (apply (car processor) current-columns (cdr processor))) fill-processor-list))))
    (end-of-line)
    (insert fill-result)))

;;;###autoload
(defun flash-fill-region ()
  "Automating fill selected region base on region beginning,
which number of columns is same.
e.g, active region is
`
flash fill,flash-fill
emacs config,emacs-config
gnu software,
free software,
'

This will fill last two line, using first and second line to guess what you want to do.
With the default config, the result will be
`
flash fill,flash-fill
emacs config,emacs-config
gnu software,gnu-software
free software,free-software
'
"
  (interactive)
  (when (region-active-p)
    (save-excursion
      (when (> (region-beginning) (region-end))
	(exchange-point-and-mark))
      (let ((start-point (region-beginning))
	    (end-line (line-number-at-pos (region-end)))
	    fill-processor-list)
	(goto-char (region-end))
	(if (= (current-column) 0)
	    (setq end-line (- end-line 1)))
	(goto-char start-point)

	(cl-do* ((current-columns (flash-fill-collect-line-columns) (flash-fill-collect-line-columns))
		 (example-length (length current-columns))
		 example-rows)
	    ((or (> (line-number-at-pos) end-line)
		 (= (point) (point-max))))
	  (beginning-of-line)
	  (cond
	   ((and (not fill-processor-list)
		 (>= (length current-columns) example-length))
	    (setq example-rows (append example-rows (list current-columns))))
	   ((not fill-processor-list)
	    (setq fill-processor-list (flash-fill-make-processor-for-region (length current-columns) example-rows))
	    ))
	  
	  (when fill-processor-list
	    (end-of-line)
	    (insert (string-join (mapcar (lambda (processor) (apply (car processor) current-columns (cdr processor))) fill-processor-list))))
	  
	  (forward-line))))))

;;;###autoload
(defun flash-fill-region-or-line ()
  "Call `flash-fill-region' if region is actived.
Otherwise, call `flash-fill-line'"
  (interactive)
  (if (region-active-p)
      (flash-fill-region)
    (flash-fill-line)))

(provide 'flash-fill)

;;; flash-fill.el ends here
