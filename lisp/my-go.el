;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    GO                                            ;;
(require 'go-autocomplete)
(require 'go-flymake)
;;(require 'go-flycheck)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(require 'go-dlv)

;;;;;;;; end of go ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
