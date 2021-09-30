;; UI Customization
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)

(setq inhibit-startup-screen t)

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
(set-face-attribute 'default nil :font "Fira Code" :height 155)
(set-face-attribute 'mode-line nil :height 140)
(set-face-attribute 'mode-line-inactive nil :height 140)

(setq display-fill-column-indicator t)

;; highlight the current line
(global-hl-line-mode +1)

;; Behaviour

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Packages

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

;; install use-package
(quelpa 'use-package)


(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(use-package nano-theme
  :ensure nil
  :defer t
  :quelpa (nano-theme
           :fetcher github
           :repo "rougier/nano-theme"))

;; nano theme
(load-theme 'nano t)


;; language support

; markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
