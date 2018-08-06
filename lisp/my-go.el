;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    GO                                            ;;
(require 'go-autocomplete)
(require 'go-flymake)
;;(require 'go-flycheck)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(require 'go-dlv)

(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

;;;;;;;; end of go ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
