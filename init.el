(require 'package)
;;(add-to-list 'package-archives
;;             '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

 (setq package-archives '(("gnu"   . "http://1.15.88.122/gnu/")
                          ("melpa" . "http://1.15.88.122/melpa/")
			  ("org"   . "http://1.15.88.122/org/")))

(package-initialize)

(set-default-coding-systems 'utf-8)
(define-coding-system-alias 'UTF-8 'utf-8)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)

(setq custom-file (concat user-emacs-directory "custom.el"))

(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'my-package)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(when (memq window-system '(mac ns))
  (set-fontset-font t nil (font-spec :family "Apple Color Emoji") nil 'append))

(require 'exec-path-from-shell)
(setq exec-path-from-shell-variables
      (append exec-path-from-shell-variables '("LC_ALL" "LANG" "GOPATH" "GEM_HOME" "GEM_PATH" "JAVA_HOME")))
(exec-path-from-shell-initialize)


(require 'global)
(require 'my-command)
(require 'my-org)
(require 'my-go)
(require 'my-haskell)
(require 'my-agda)
(require 'my-scheme)
(require 'my-clojure)
(require 'my-ruby)
(require 'my-python)
(require 'my-rust)
(require 'my-cc)
(require 'my-octave)
(require 'my-json)
(require 'my-web)
;;(require 'my-ejc)
(require 'my-latex)
(require 'my-liquid)
(require 'my-protobuf)
(require 'my-elm)
(require 'my-scala)
(require 'my-databaseclient)
(require 'theme-settig)

;; load private setting, like passowd, token etc
(defconst private-setting-dir "~/.emacs.d/private/")
(if (file-directory-p private-setting-dir)
    (progn
      (let ((pfs (directory-files private-setting-dir)))
	(progn
	  (dolist (f pfs)
	    (let ((l (concat private-setting-dir f)))
	      (if (file-regular-p l)
		  (load-file l))))))))

(load custom-file 'noerror)
