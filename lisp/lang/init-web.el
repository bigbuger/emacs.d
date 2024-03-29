;;; init-web.el --- wirite web setting

;;; Commentary:
;; 
;;; Code:

(require 'web-mode)
(require 'company-web-html)


(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(dolist (mode (list 'web-mode-hook 'html-mode-hook))
  (add-hook mode
            (lambda () (setq-local company-backends
				   (cl-adjoin '(company-web-html company-css :with company-yasnippet) company-backends :test #'equal)))))


(provide 'init-web)

;;; init-web.el ends here
