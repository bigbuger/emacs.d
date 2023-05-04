;;; my-clojure.el --- my clojure setting



;;; Commentary:
;; 
;;; Code:
(require 'clojure-mode)
(require 'clj-refactor)
(require 'cider)


;; (add-hook 'clojure-mode-hook
;; 	  #'(lambda ()
;; 	      (setq-local lsp-enable-indentation nil)
;; 	      (lsp-deferred)))
(add-hook 'clojure-mode-hook 'clj-refactor-mode)

(provide 'my-clojure)

;;; my-clojure.el ends here
