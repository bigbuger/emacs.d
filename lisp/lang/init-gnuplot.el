;;; init-gnuplot.el --- gnuplot setting

;;; Commentary:
;; 

;; gnuplot 用来给表格画图
(use-package gnuplot
  :ensure t
  :init
  ;; 通过 gnuplot 源码中的 doc/doc2texi.el 生成 gnuplot-eldoc 和 gnuplot.info:
  ;; #+begin_src
  ;; emacs -batch -l doc2texi.el -f d2t-doc-to-texi
  ;; makeinfo gnuplot,info --no-split
  ;; #+end_src
  (load-file "~/.emacs.d/lisp/libs/gnuplot-eldoc.el")
  (setq gnuplot-eldoc-mode t)
  (with-eval-after-load 'org
    (add-to-list 'org-babel-load-languages
		 '(gnuplot . t))))

(provide 'init-gnuplot)

;;; init-gnuplot.el ends here
