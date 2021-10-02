;; Behaviour

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; store all backup and autosave files in the tmp directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; smart parentheses
(require 'smartparens-config)
(smartparens-global-mode t)


(provide 'behaviour)

