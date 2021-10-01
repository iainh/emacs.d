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

;; Auto-compile
(use-package auto-compile
  :ensure t
  :defer t)

;; magit for Git 
(use-package magit
  :ensure t
  :defer t)

;; (use-package nano-theme
;;   :ensure nil
;;   :defer t
;;   :quelpa (nano-theme
;;            :fetcher github
;;            :repo "rougier/nano-theme"))

;; minor mode to display the key bindings following your currently entered
;; incomplete command in a popup.
(use-package which-key
  :ensure t
  :defer t)

;; Smartparens is a minor mode for dealing with pairs in Emacs.
(use-package smartparens
  :ensure t
  :defer t)

;; Ensure environment variables inside Emacs look the same as in the user's shell
(use-package exec-path-from-shell
  :ensure t
  :defer t)

; markdown
(use-package markdown-mode
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(provide 'packages)
