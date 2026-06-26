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

(use-package multi-vterm :ensure t)

(provide 'init-vterm)

;;; init-vterm.el ends here
