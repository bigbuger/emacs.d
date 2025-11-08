;;; init-vterm.el --- vterm setting
;;; Commentary:
;; 

;;; Code:

(require 'vterm)
(global-set-key (kbd "C-c t") 'vterm-other-window)

(defun vterm-left-word ()
  (interactive)
  (vterm-send "M-b"))

(defun vterm-right-word ()
  (interactive)
  (vterm-send "M-f"))

(define-key vterm-mode-map (kbd "M-<left>") #'vterm-left-word)
(define-key vterm-mode-map (kbd "M-<right>") #'vterm-right-word)
(define-key vterm-mode-map (kbd "M-<up>") #'vterm-send-M-p)
(define-key vterm-mode-map (kbd "M-<down>") #'vterm-send-M-n)

(defun my-append-to-buffer (bf text)
  (with-current-buffer bf
    (save-excursion
      (end-of-buffer)
      (insert text "\n"))))

;; #+BEGIN_SRC shell
;; if [[ "$INSIDE_EMACS" = 'vterm' ]] \
;;     && [[ -n ${EMACS_VTERM_PATH} ]] \
;;     && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh ]]; then
;;     source ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh
;; 
;;     e() {
;; 	vterm_cmd find-file-other-window "$(realpath "${@:-.}")"
;;     }
;; 
;;     bf () {
;; 	buffer="$1"
;; 	while read -r input; do
;; 	    vterm_cmd my-append-to-buffer "${buffer}" "${input}"
;; 	done
;;     }
;; fi
;; #+END_SRC

(add-to-list 'vterm-eval-cmds '("find-file-other-window" find-file-other-window))
(add-to-list 'vterm-eval-cmds '("my-append-to-buffer" my-append-to-buffer))

(with-eval-after-load 'projectile
  (defun projectile-run-vterm-other-window (&optional arg)
    "Invoke `vterm-other-window' in the project's root.

Switch to the project specific term buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead."
    (interactive "P")
    (let* ((project (projectile-acquire-root))
           (buffer (projectile-generate-process-name "vterm" arg project)))
      (unless (buffer-live-p (get-buffer buffer))
	(unless (require 'vterm nil 'noerror)
          (error "Package 'vterm' is not available"))
	(projectile-with-default-dir project
          (vterm-other-window buffer)))
      (switch-to-buffer buffer)))

  (define-key projectile-command-map (kbd "t") 'projectile-run-vterm-other-window))

(use-package multi-vterm :ensure t)

(provide 'init-vterm)

;;; init-vterm.el ends here
