;;;

(require 'elpy)
(require 'company-jedi)

;; enable elpy jedi backend
(setq elpy-rpc-backend "jedi")

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(elpy-enable)
(add-hook 'python-mode-hook 'jedi-mode)
;;;
