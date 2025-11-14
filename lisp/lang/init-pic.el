;;; init-pic.el --- pic little language for draw graph.  The classic source is Brian Kernighan’s original Bell Labs Computing Science Technical Report No. 116 on the language from 1991


;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp/libs/pic-ts-mode")

;;; Commentary:
;; 

(require 'pic-ts-mode)
(pic-ts-mode-install-grammar)
;; (add-to-list 'org-src-lang-modes '("pic" . pic-ts)) ;; fixme

(use-package ob-pic)

(provide 'init-pic)

;;; init-pic.el ends here
