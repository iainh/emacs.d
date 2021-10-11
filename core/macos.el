;; On macOS Emacs doesn't use the shell PATH if it's not started from
;; the shell. Let's fix that:

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(when window-system
  ;; macOS specific UI configuration
  (use-package ns-auto-titlebar
    :init (ns-auto-titlebar-mode))
  (set-fontset-font t 'symbol (font-spec :family "Apple Symbols") nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))


;; macOS ls doesn't support '-X' or arguments like '--sort' so use coreutils ls
;; from macPorts
(when (equal system-type 'darwin)
  (setq insert-directory-program "/opt/local/bin/gls"))

(provide 'macos)
;;; end of macos.el
