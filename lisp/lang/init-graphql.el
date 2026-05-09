;;; init-graphql.el --- graphql


;;; Commentary:
;; 

;;; Code:
(use-package graphql-ts-mode
  :ensure t
  :mode ("\\.graphql\\'" "\\.gql\\'")
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(graphql "https://github.com/bkegley/tree-sitter-graphql"))))

(provide 'init-graphql)

;;; init-graphql.el ends here
