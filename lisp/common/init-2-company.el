;;; init-company.el --- company 自动补全插件配置



;;; Commentary:
;; 

;;; Code:

(require 'company)
(global-company-mode 1)
(setq company-minimum-prefix-length 1)
(setq company-require-match 'never)
(setq company-show-quick-access 'left)
(add-hook 'after-init-hook 'company-quickhelp-mode)

(global-set-key (kbd "<backtab>") 'company-complete)
(global-set-key (kbd "<C-S-tab>") 'company-files)

(add-to-list 'load-path "~/.emacs.d/lisp/libs/company-english-helper")
(require 'company-english-helper)
;; (global-set-key (kbd "<C-M-tab>") 'company-english-helper-search)

(setq company-backends
      '(company-semantic
	company-cmake
	company-capf
	company-clang
	company-files
	(company-dabbrev-code company-gtags company-etags company-keywords)
	company-dabbrev))
(setq company-dabbrev-char-regexp "[A-z]") ;; 限定 dabbre 只补全英文

;; make company combine with yasnippet
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  "Add company-yasnippet after BACKEND."
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
(setq company-backends (append company-backends '(company-yasnippet)))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") #'company-select-next)
  (define-key company-active-map (kbd "M-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-n") nil)
  (define-key company-active-map (kbd "C-p") nil))

(provide 'init-2-company)

;;; init-2-company.el ends here
