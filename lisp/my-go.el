;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    GO                                            ;;

;;(require 'go-autocomplete)

(require 'go-dlv)
(require 'go-guru)
(require 'go-rename)

(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

(add-hook 'go-mode-hook
	  (lambda ()
	    (setq-local company-backends '((company-go :with company-yasnippet)
					    (company-files :with company-yasnippet)
					    (company-dabbrev :with company-yasnippet)))))

;;(require 'go-flymake)
;;(require 'go-flycheck)
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook 'go-eldoc-setup)


(define-key go-mode-map
  (kbd "M-.") 'godef-jump)

(add-hook 'go-mode-hook (lambda ()
			  (setq-local flycheck-disabled-checkers '(go-staticcheck))))

;;;;;;;; end of go ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
