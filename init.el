(require 'package)

(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
			 ;;("org"   . "http://1.15.88.122/org/")
			 ))

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
  (setq mouse-wheel-flip-direction t)
  (set-fontset-font t nil (font-spec :family "Apple Color Emoji") nil 'append))

;; special set for emacs port
(when (boundp 'mac-carbon-version-string)
  (load "~/.emacs.d/init-for-emacsport" 'noerror))

(require 'exec-path-from-shell)
(setq exec-path-from-shell-variables
      (append exec-path-from-shell-variables '("LC_ALL" "LANG" "GOPATH" "GEM_HOME" "GEM_PATH" "JAVA_HOME" "LIBGS" "GPG_TTY")))
(exec-path-from-shell-initialize)


;; Keep Folders Clean
(setq no-littering-var-directory
      (expand-file-name "data/" "~/.cache/emacs"))
(use-package no-littering
  :ensure t
  :demand t)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(defun load-dir (dir)
  "Load all file from DIR, not recursively."
  (progn
    (add-to-list 'load-path dir)
    (let ((files (directory-files dir nil "^init-.*\\.el$")))
      (dolist (f files)
	(require (intern (file-name-base f)))))))


(require 'my-command)
(load-dir "~/.emacs.d/lisp/common")
(load-dir "~/.emacs.d/lisp/lang")
(load-dir "~/.emacs.d/lisp/org-config")

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
		  (load  "~/.emacs.d/lisp/theme-setting.el"))))
  (load "~/.emacs.d/lisp/theme-setting.el"))



;; load private setting, like passowd, token etc
(defconst private-setting-dir "~/.emacs.d/private/")
(if (file-directory-p private-setting-dir)
    (let ((pfs (directory-files private-setting-dir nil "\\.el$")))
      (dolist (f pfs)
	(let ((l (concat private-setting-dir f)))
	  (load-file l)))))

(load custom-file 'noerror)
