;;; init-http-client.el --- http client, aka curl
;; verb 网络请求客户端

;;; Commentary:
;; 

;;; Code:

(use-package verb
  :demand t
  :config
  (define-key verb-response-body-mode-map (kbd "C-c i") #'consult-jq)
  (setq verb-tag "curl"
	verb-auto-kill-response-buffers t)
  (add-to-list 'display-buffer-alist
	       `("\\*HTTP Response.*\\*"
		 (display-buffer-reuse-window display-buffer-in-direction)
		 (direction . right)
		 (dedicated . t)))
  
  :init
  (add-to-list 'load-path "~/.emacs.d/lisp/libs/curl-to-elisp")
  (require 'curl-to-elisp)
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

  (defun verb-graphql (rs)
    "Converts Verb-mode's request object RS into a GraphQL request.
For use with Verb-Map-Request property."
  (pcase-let* ((`(,query ,variables) (split-string (oref rs body) ":variables:"))
               (body `(:query ,query :variables ,(json-parse-string (or variables "{}")))))
    (oset rs body (json-encode body)))
  rs))

(use-package impostman
  :ensure t)

(provide 'init-http-client)

;;; init-http-client.el ends here
