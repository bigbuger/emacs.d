;;; my-json.el --- my json mode config


;;; Commentary:
;; 

;;; Code:
(require 'json-mode)
(require 'counsel-jq)

;; reqquier fx: https://github.com/antonmedv/fx
;; install by: npm install -g fx
;;(setq counsel-jq-command "fx")

(add-hook 'json-mode-hook 'hideshowvis-enable)
(define-key json-mode-map (kbd "C-c j") 'counsel-jq)


(provide 'my-json)

;;; my-json.el ends here
