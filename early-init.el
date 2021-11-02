;;; early-init.el -*- lexical-binding: t; -*-

;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

;; For LSP, increase the amount of data that Emacs can read from a process.
;; Some language server responses can be 800k to 3M
(setq read-process-output-max (* 1024 1024)) ;; 1M

;; Disable package.el in favour of straight.el
(setq package-enable-at-startup nil)

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

;; (load-theme 'modus-vivendi t)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
;; (push '(menu-bar-lines . 0) default-frame-alist)
;; (push '(tool-bar-lines . 0) default-frame-alist)
;; (push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default. We will set the initial frame size to something
;; larger in core/behaviour.el.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; size the frame so something a little more usable to start
(setq default-frame-alist
       '((height . 60)
         (width . 120)
         (left . 40)
         (top . 40)
         (vertical-scroll-bars . nil)
         (horizontal-scroll-bars . nil)
         (tool-bar-lines . 0)))

(unless (or (daemonp) noninteractive)
  ;; Emacs really shouldn't be displaying anything until it has fully started
  ;; up. This saves a bit of time.
  (setq-default inhibit-redisplay t
		inhibit-message t)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redisplay))))

;; UTF-8 is a sensible starting point on all platforms
(set-language-environment "UTF-8")

(provide 'early-init)
