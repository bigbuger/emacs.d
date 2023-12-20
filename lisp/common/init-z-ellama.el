;;; init-ellama.el --- ellama, ollama 大模型

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
  (setq-default gptel-model "zephyr:latest"
		gptel-backend (gptel-make-ollama
		 "Ollama"                               ;Any name of your choosing
		 :host "localhost:11434"                ;Where it's running
		 :models '("zephyr:latest")            ;Installed models
		 :stream t)))


;;; init-ellama.el ends here
