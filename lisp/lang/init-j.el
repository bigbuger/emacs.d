;;; init-j.el --- J language setting

;; Add this to your emacs config

;;; Commentary:
;; 

;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp/libs/j-mode")
(autoload 'j-mode "j-mode.el" "Major mode for editing J files" t)
(setq j-console-cmd "jconsole")
(add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . j-mode))

(require 'ob-J)

(with-eval-after-load 'org
  (add-to-list 'org-src-lang-modes '("J" . j))
  (add-to-list 'org-babel-load-languages
	       '(J . t)))

(provide 'init-j)

;;; init-j.el ends here
