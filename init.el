(require 'package)

(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
			 ("org"   . "http://1.15.88.122/org/")))

(package-initialize)

(set-default-coding-systems 'utf-8)
(define-coding-system-alias 'UTF-8 'utf-8)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)

(setq custom-file (concat user-emacs-directory "custom.el"))

(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'init-package)

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
(require 'init-org)
(require 'init-go)
(require 'init-haskell)
(require 'init-agda)
(require 'init-scheme)
(require 'init-clojure)
(require 'init-ruby)
(require 'init-python)
(require 'init-rust)
(require 'init-cc)
(require 'init-octave)
(require 'init-json)
(require 'init-web)
;;(require 'init-ejc)
(require 'init-latex)
(require 'init-liquid)
(require 'init-protobuf)
(require 'init-elm)
(require 'init-scala)
(require 'init-databaseclient)
(require 'init-calendar)
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
