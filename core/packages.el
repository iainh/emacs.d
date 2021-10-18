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

;; -- Packages --
(use-package crux
  :defer t)

(use-package buffer-flip
  :defer t
  :bind (("C-<tab>" . buffer-flip)
	 :map buffer-flip-map
          ( "C-<tab>" .   buffer-flip-forward) 
          ( "C-S-<tab>" . buffer-flip-backward) 
          ( "C-ESC" .     buffer-flip-abort))
  :config
  (setq buffer-flip-skip-patterns
        '("^\\*helm\\b"
          "^\\*swiper\\*$")))

(use-package dired
  :straight (:type built-in)
  :custom (dired-listing-switches "-aGBhlp --group-directories-first"))

(use-package exec-path-from-shell
  :defer t)

(use-package smartparens
  :defer t
  :diminish smartparens-mode
  :init (smartparens-global-mode)
  :hook (minibuffer-setup . smartparens-mode)
  :bind (:map emacs-lisp-mode-map
	      (";" . sp-comment))
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
  (sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET"))))

(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Show diff highlight in the gutter
(use-package diff-hl
  :defer 1
  :hook
  (dired-mode . diff-hl-dired-mode-unless-remote)
  :config
  (global-diff-hl-mode 1))

(use-package magit
  :bind ("C-x g" . magit-status)
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package which-key
  :diminish
  :config (which-key-mode))

(use-package lsp-mode
  :defer t
  :config (setq lsp-headerline-breadcrumb-enable nil))

(use-package yasnippet
  :defer t
  :diminish
  :config (yas-reload-all)
  :hook ((prog-mode . yas-minor-mode)
	 (text-mode . yas-minor-mode)))

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
  "So that run C-c C-c C-r works without having to confirm, but don't try to
  save rust buffers that are not file visiting."
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(use-package company
  :diminish company
  :bind (:map company-active-map
	      ("<tab>"    . nil)
	      ("TAB"      . company-complete-selection)
	      ("<escape>" . company-abort))
  :config (setq
	   company-idle-delay 0.1
	   company-minimum-prefix-length 1
	   company-selection-wrap-around t
	   company-tooltip-align-annotations t
	   company-show-quick-access nil
	   company-tooltip-limit 8
	   company-backends '((
			       company-capf
			       company-semantic
			       ;;company-files
			       company-keywords
			       company-yasnippet
			       )))
  :hook (after-init . global-company-mode))

(use-package company-box
  :defer t
  :diminish company-box-mode
  :after company
  :hook
  (company-mode . company-box-mode))

(use-package selectrum
  :init (selectrum-mode +1))

(use-package prescient
  :config (prescient-persist-mode +1))

(use-package selectrum-prescient
  :after selectrum
  :init (selectrum-prescient-mode +1))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

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
  ;;(load-theme 'modus-operandi t)
  ;; Swap between light and dark
  :bind ("<f5>" . modus-themes-toggle))

(use-package acme-theme
  :defer t
  :init (load-theme 'acme t))

(use-package doom-modeline
  :config (doom-modeline-mode)
  ;; Reduce the scale factor for icons from 1.2 to 1.1 to fix the text
  ;; on the right edge being cut off when the scrollbar is disabled.
  ;; https://github.com/hlissner/doom-emacs/issues/2967
  (setq all-the-icons-scale-factor 1.0
	;;doom-modeline-height -1
	doom-modeline-icon 'nil))

(provide 'packages)
;;; end of packages
