;;; init-llm.el --- llm 大模型

;;; Commentary:
;; 

;;; Code:

(use-package ellama
  :init
  (setq ellama-keymap-prefix nil)
  (setopt ellama-language "Chinese")
  (require 'llm-ollama))


(use-package gptel
  :init
  (setq-default gptel-model "gemma:2b"
		gptel-backend (gptel-make-ollama
		 "Ollama"                               ;Any name of your choosing
		 :host "localhost:11434"                ;Where it's running
		 :models '("gemma:2b")            ;Installed models
		 :stream t)))

(provide 'init-z-llm)

;;; init-z-ellama.el ends here
