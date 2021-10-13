;; Install packages using straight.el

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package and configure to always use straight.el
(straight-use-package 'use-package)
(use-package straight
	     :custom (straight-use-package-by-default t))

(use-package crux
  :defer t)

(use-package dired
  :straight (:type built-in)
  :custom (dired-listing-switches "-aBhl --group-directories-first"))

(use-package exec-path-from-shell
  :defer t)

(use-package smartparens
  :defer t
  :diminish smartparens-mode
  :init (smartparens-global-mode)
  :hook (minibuffer-setup . smartparens-mode)
  :config
  (require 'smartparens-config)
  ;; Enter in parens should create a new empty line that is properly indented
  (defun indent-between-pair (&rest _ignored)
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
  (sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET")))

  ;; Bind ";" to sp-comment in elisp
  (bind-key ";" 'sp-comment emacs-lisp-mode-map)
)
(use-package markdown-mode
  :defer t)

;; Show diff highlight in the gutter
;; (use-package diff-hl
;;   :defer 1
;;   :hook
;;   (dired-mode . diff-hl-dired-mode-unless-remote)
;;   :config
;;   (global-diff-hl-mode 1))

(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status))
  ;; :hook
  ;; (magit-pre-refresh . diff-hl-magit-pre-refresh)
  ;; (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package which-key
  :diminish
  :config (which-key-mode))

(use-package lsp-mode
  :defer t)
(use-package yasnippet
  :defer t
  :diminish
  :config (yas-reload-all)
  :hook ((prog-mode . yas-minor-mode)
	 (text-mode . yas-minor-mode)))

(use-package yasnippet-snippets)

(use-package rustic
  :defer t
  :config
  (setq rustic-lsp-server 'rust-analyzer
	lsp-eldoc-hook nil
	lsp-enable-symbol-highlight t
	lsp-signature-auto-activate nil
	;; comment to disable rustfmt on save
	rustic-format-on-save t)
  :hook (rustic-mode . rk/rustic-mode-hook))
  
(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(use-package ivy
  :config (ivy-mode t)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-enable-recursive-minibuffers t)
  (ivy-re-builders-alist '((t . ivy--regex-plus))))

(use-package company
  :diminish company-mode
  :bind (:map company-active-map
	      ("<tab>" . nil)
	      ("TAB"   . company-complete-selection))
  :config (setq
	   company-idle-delay 0.05
	   company-minimum-prefix-length 1
	   company-selection-wrap-around t
	   ;; backends
	   company-backends '((
			       company-capf
			       company-semantic
			       ;;company-files
			       company-keywords
			       )))
  :hook (after-init . global-company-mode))

(use-package company-posframe
  :defer t
  :after company
  :config
  (company-posframe-mode))

(use-package counsel
  :init (counsel-mode))

(use-package flx
  :defer t
  :after ivy counsel)

(use-package ivy-posframe
  :defer 1
  :after ivy
  :config
  (setq ivy-posframe-display-functions-alist
        '((t . ivy-posframe-display)))
  (ivy-posframe-mode))

(use-package flycheck
  :defer t
  :hook (after-init . global-flycheck-mode))

(use-package spacemacs-theme
  :defer t)
(use-package doom-themes
  :defer t)
(use-package modus-themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
	modus-themes-bold-constructs nil)
 
  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  ;;(load-theme 'modus-vivendi t)
  (load-theme 'modus-operandi t)
  ;; Swap between light and dark
  :bind ("<f5>" . modus-themes-toggle))

(use-package doom-modeline
  :config (doom-modeline-mode)
  ;; Reduce the scale factor for icons from 1.2 to 1.1 to fix the text
  ;; on the right edge being cut off when the scrollbar is disabled.
  ;; https://github.com/hlissner/doom-emacs/issues/2967
  (setq all-the-icons-scale-factor 1.1
	doom-modeline-height 1
	doom-modeline-icon 'nil))

(provide 'packages)
;;; end of packages
