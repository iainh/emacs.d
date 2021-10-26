;; Behaviour

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; size the frame so something a little more usable to start
(when window-system
  (set-frame-position (selected-frame) 40 40)
  (set-frame-size (selected-frame) 91 43))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Use ibuffer over the default.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Enable the built-in Windmove keys. Use meta rather than shift since at
;; times muscle memory still has be changing the selection with <shift>-<arrow>
(windmove-default-keybindings 'meta)

;; Allow killing the current buffer without confirmation. Generally
;; this is the behaviour that I expect so being prompted is a nuisance.
(defun ih/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'ih/kill-this-buffer)

;; store all backup and autosave files in the tmp directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; requires that crux be installed
(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key (kbd "C-c o") #'crux-open-with)
(global-set-key [(shift return)] #'crux-smart-open-line)
(global-set-key (kbd "s-r") #'crux-recentf-find-file)
(global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
(global-set-key [remap kill-whole-line] #'crux-kill-whole-line)

;; Text mode customizations
(use-package text-mode
  :straight (:type built-in)
  :hook (text-mode . turn-on-visual-line-mode))

;; show trailing whitespace
(setq show-trailing-whitespace t)

(provide 'behaviour)

