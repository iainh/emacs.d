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

;; More useful frame title, that show either a file or a buffer name
(setq frame-title-format
      '("" "Emacs " emacs-version " " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; Cursor configuration
(setq-default cursor-type 'box)

;; Show line numbers at the beginning of each line in prog-mode only.
(use-package prog-mode
  :straight (:type built-in)
  :hook ((prog-mode . display-line-numbers-mode)
	 (prog-mode . hl-line-mode)))

(when window-system
  ;; Font configuration. Check for the presence of 'Source Code Pro' before setting it.
  ;; If it is not present, use 'monospace'.
  (if (member "Fira Code" (font-family-list))
      (set-face-attribute 'default nil :font "Fira Code" :weight 'normal :height 160)
    (set-face-attribute 'default nil :font "monospace" :height 140))

  ;; A hack to vertically centre the text on a line until emacs supports the
  ;; true centering. Patch by Jesse Medeiros started 2019:
  ;;   https://yhetil.org/emacs-devel/87eeewak2c.fsf@gnus.org/T/#u
  (defun set-bigger-spacing ()
    (setq-local default-text-properties '(line-spacing 0.125 line-height 1.125)))
  (add-hook 'text-mode-hook 'set-bigger-spacing)
  (add-hook 'prog-mode-hook 'set-bigger-spacing))

;; Theme setting. Attempt to do the right thing when a window is not present
;; and sent a dark theme to fit it with my dark terminal sessions.
(if window-system (load-theme 'modus-operandi t)
  (load-theme 'modus-vivendi t))

(provide 'ui)
;;; ui.el ends here
