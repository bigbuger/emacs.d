;;; my-json.el --- my json mode config


;;; Commentary:
;; 

;;; Code:
(require 'json-mode)

(add-to-list 'load-path "~/.emacs.d/lisp/counsel-jq-ex/")
(require 'counsel-jq-ex)

(add-hook 'json-mode-hook 'hideshowvis-enable)
(define-key json-mode-map (kbd "C-c j") 'counsel-jq-ex)
(define-key js-mode-map (kbd "C-c j") 'counsel-jq-ex)



(provide 'my-json)

;;; my-json.el ends here
