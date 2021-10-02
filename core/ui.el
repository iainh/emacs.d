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
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; show line numbers at the beginning of each line
(if (fboundp 'global-display-line-numbers-mode)
    (global-display-line-numbers-mode)
  (global-nlinum-mode t))


;; font
(set-face-attribute 'default nil :font "IBM Plex Mono" :height 150)
(set-face-attribute 'mode-line nil :height 140)
(set-face-attribute 'mode-line-inactive nil :height 140)

(setq display-fill-column-indicator t)

;; highlight the current line
(global-hl-line-mode +1)

;; show available keybindings after you start typing
(require 'which-key)
(which-key-mode +1)

(provide 'ui)
;;; end of ui.el
