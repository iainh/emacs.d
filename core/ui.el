;; UI Customization
(setq inhibit-startup-screen t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" "Emacs " emacs-version " " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; cursor: Use a red bar cursor rather than the standard block
(setq-default cursor-type 'bar)
(set-cursor-color "#ff0000")

;; show line numbers at the beginning of each line
(if (fboundp 'global-display-line-numbers-mode)
    (global-display-line-numbers-mode)
  (global-nlinum-mode t))

(when window-system
  ;; Font configuration
  (set-face-attribute 'default nil :font "Inconsolata" :height 180)
  (set-face-attribute 'mode-line nil :height 130)
  (set-face-attribute 'mode-line-inactive nil :height 130))

;; highlight the current line
(global-hl-line-mode +1)

;; show available keybindings after you start typing
(require 'which-key)
(which-key-mode +1)

(provide 'ui)
;;; end of ui.el
