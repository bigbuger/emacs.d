;;; init-ellama.el --- ellama, ollama 大模型

;;; Commentary:
;; 

;;; Code:

(use-package ellama
  :init
  (setq ellama-keymap-prefix nil)
  (setopt ellama-language "Chinese")
  (require 'llm-ollama))



;;; init-ellama.el ends here
