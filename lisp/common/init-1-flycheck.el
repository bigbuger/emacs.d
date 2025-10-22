;;; init-flycheck.el --- flycheck setting
;;; Commentary:
;; 

;;; Code:

(require 'flycheck)

(setq flycheck-global-modes '(not org-mode))
(global-flycheck-mode)

(add-to-list 'load-path "~/.emacs.d/lisp/libs/flycheck-posframe")
(require 'flycheck-posframe)
(setq flycheck-posframe-warning-prefix "⚠ ")
(setq flycheck-posframe-info-prefix "ℹ ")
(setq flycheck-posframe-error-prefix "⨯ ")

(setq flycheck-posframe-timeout 4)
(with-eval-after-load 'flycheck
  (require 'flycheck-posframe)
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(setq flycheck-indication-mode 'right-fringe)

(defvar-local flycheck-local-checkers nil)
(defun +flycheck-checker-get(fn checker property)
  (or (alist-get property (alist-get checker flycheck-local-checkers))
      (funcall fn checker property)))
(advice-add 'flycheck-checker-get :around '+flycheck-checker-get)

(setq flycheck-checker-error-threshold 600
      flycheck-highlighting-style 'level-face)

(setq flycheck-error-list-format
      [("File" 6)
       ("Line" 5 flycheck-error-list-entry-< :right-align t)
       ("Col" 3 nil :right-align t)
       ("Level" 20 flycheck-error-list-entry-level-<)
       ("ID" 20 t)
       (#("Message (Checker)" 0 7 (face flycheck-error-list-error-message) 9 16 (face flycheck-error-list-checker-name)) 0 t)])

(use-package consult-flycheck
  :after consult
  :bind (:map flycheck-mode-map
	      ("M-g e" . consult-flycheck)
	      ("M-g M-e" . consult-flycheck)))

(provide 'init-1-flycheck)

;;; init-1-flycheck.el ends here
