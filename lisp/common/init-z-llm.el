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
  (setq gptel-directives
	'((default     . "You are a large language model living in Emacs and a helpful assistant. Respond concisely in chinese.")
	  (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note. Respond concisely in chinese.")
	  (writing     . "You are a large language model and a writing assistant. Respond concisely in chinese.")
	  (chat        . "You are a large language model and a conversation partner. Respond concisely in chinese.")))
  
  (let ((my-ollama (gptel-make-ollama
		       "Ollama"                            ;Any name of your choosing
		     :host "localhost:11434"                ;Where it's running
		     :models '("deepseek-r1:8b" "gemma:2b")    ;Installed models
		     :stream t)))
    (setq
     ;; gptel-default-mode 'org-mode
     gptel-model 'deepseek-r1:8b
     gptel-backend my-ollama
     gptel--known-backends `(("Ollama" . ,my-ollama)))) ;清空其他在线后端，反正没钱买 😁
  )


(provide 'init-z-llm)

;;; init-z-ellama.el ends here
