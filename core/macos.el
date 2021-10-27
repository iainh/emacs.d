;; On macOS Emacs doesn't use the shell PATH if it's not started from
;; the shell. Let's fix that:

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(when window-system
  ;; Enable menubar on macOS
  (menu-bar-mode +1)

  ;; macOS specific UI configuration
  (use-package ns-auto-titlebar
    :init (ns-auto-titlebar-mode))

  (set-fontset-font t 'symbol (font-spec :family "Apple Symbols") nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

(defun ih/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'modus-operandi t))
    ('dark (load-theme 'modus-vivendi t))))

(add-hook 'ns-system-appearance-change-functions #'ih/apply-theme)

;; BSD ls doesn't support '--group-directories-first' from the general
;; configuration so override the switches to exclude it.
(setq dired-listing-switches "-aGBhlp")

(provide 'macos)
;;; end of macos.el
