;;; init-clojure.el --- init clojure setting



;;; Commentary:
;; 
;;; Code:
(require 'clojure-mode)
;; (require 'clj-refactor)
(require 'cider)


(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)
;; (add-hook 'clojure-mode-hook 'clj-refactor-mode)

(use-package ob-clojure
  :ensure nil
  :after org
  :config
  (setq org-babel-clojure-backend 'cider)
  (add-to-list 'org-babel-load-languages '(clojure . t)))

(provide 'init-clojure)

;;; init-clojure.el ends here
