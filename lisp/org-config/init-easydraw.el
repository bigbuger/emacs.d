;;; init-easydraw.el --- easydraw config

;;; Commentary:
;; 

;;; Code:


(add-to-list 'load-path "~/.emacs.d/lisp/libs/el-easydraw")
(with-eval-after-load 'org
  (require 'edraw-org)
  (edraw-org-setup-default))


(provide 'init-easydraw)

;;; init-easydraw.el ends here
