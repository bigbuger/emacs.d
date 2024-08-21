;;; init-raku.el --- raku (aka perl6) setting
;;; Commentary:
;; 

;;; Code:

(use-package raku-mode
  :ensure t
  :defer t)

(use-package flycheck-raku
  :ensure t)

(use-package ob-raku
  :init
  (with-eval-after-load 'org
    (add-to-list 'org-babel-load-languages
		 '(raku . t))))

(provide 'init-raku)

;;; init-raku.el ends here
