;;; my-web.el --- wirite web setting

;;; Commentary:
;; 
;;; Code:

(require 'web-mode)
(require 'company-web-html)


(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(eval-after-load 'company
  #'(dolist (backend (list 'company-web-html 'comapny-css))
      (add-to-list 'company-backends `(,backend :with company-yasnippet))))

(provide 'my-web)

;;; my-web.el ends here
