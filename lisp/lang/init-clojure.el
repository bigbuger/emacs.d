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

(provide 'init-clojure)

;;; init-clojure.el ends here
