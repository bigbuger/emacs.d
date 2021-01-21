;;; my-python.el --- pytohon config
;;;

;;; Commentary:
;; 

;;; Code:

(require 'elpy)
(require 'company-jedi)

;; fixme
(setq python-shell-completion-native-enable nil)

;; enable elpy jedi backend
(setq elpy-rpc-backend "jedi")

(setq elpy-rpc-python-command "python3")
(setq python-shell-interpreter "python3")
;;(setq python-check-command "~/Library/Python/3.7/bin/flake8")
;;(setq flycheck-python-flake8-executable "/usr/local/bin/flake8")

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(elpy-enable)
(add-hook 'python-mode-hook 'jedi-mode)
;;;

(provide 'my-python)

;;; my-python.el ends here
