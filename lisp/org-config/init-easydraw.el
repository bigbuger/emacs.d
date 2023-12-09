;;; init-easydraw.el --- easydraw config

;;; Commentary:
;; 

;;; Code:


(add-to-list 'load-path "~/.emacs.d/lisp/libs/el-easydraw")
(with-eval-after-load 'org
  (require 'edraw-org)
  (edraw-org-setup-default))


(autoload 'edraw-color-picker-replace-color-at "edraw-color-picker" nil t)
(autoload 'edraw-color-picker-replace-or-insert-color-at-point "edraw-color-picker" nil t)

(defun my-edraw-color-picker-add-keys (map)
  ;; Replaces the color of the clicked location
  (define-key map [mouse-1] #'edraw-color-picker-replace-color-at)
  ;; C-c C-o replaces the color in place or adds color
  (define-key map (kbd "C-c C-o")
              #'edraw-color-picker-replace-or-insert-color-at-point))

(defun my-edraw-color-picker-enable ()
  (my-edraw-color-picker-add-keys (or (current-local-map)
                                      (let ((map (make-sparse-keymap)))
                                        (use-local-map map)
                                        map))))

(add-hook 'css-mode-hook 'my-edraw-color-picker-enable)
(add-hook 'mhtml-mode-hook 'my-edraw-color-picker-enable)

(defun my-edraw-color-picker-enable-for-custom-mode ()
  ;; Use emacs color name
  (setq-local edraw-color-picker-insert-default-color-scheme 'emacs))

(add-hook 'Custom-mode-hook 'my-edraw-color-picker-enable-for-custom-mode)

(with-eval-after-load "cus-edit"
  ;; Add keys to the field key map
  (my-edraw-color-picker-add-keys custom-field-keymap))

(provide 'init-easydraw)

;;; init-easydraw.el ends here
