;; Behaviour

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; size the frame so something a little more usable to start
(when window-system
  (set-frame-position (selected-frame) 40 40)
  (set-frame-size (selected-frame) 91 43))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

;; show trailing whitespace
(setq show-trailing-whitespace t)

;; Cycle forward in buffers with <ctrl>+<tab>, allowing repeats 
(require 'misc-cmds)
;;(global-set-key [remap previous-buffer] 'previous-buffer-repeat)
;;(global-set-key [remap next-buffer]     'next-buffer-repeat)
(global-set-key (kbd "C-<tab>") #'next-buffer-repeat)

(provide 'behaviour)

