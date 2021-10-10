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

(use-package crux)

(use-package exec-path-from-shell)
(use-package smartparens)
(use-package markdown-mode)
(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status))
;;(use-package restclient)	       
(use-package which-key
  :diminish)

(use-package lsp-mode)
(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))
(use-package yasnippet-snippets)

(use-package rustic
  :config
  (setq rustic-lsp-server 'rust-analyzer)
  (setq lsp-eldoc-hook nil)
  (setq lsp-enable-symbol-highlight t)
  (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;;  has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

;;(use-package projectile)
(use-package ivy
  :init (ivy-mode t))

(use-package company
  :demand t
  :diminish company-mode
  :bind (("M-/"  .  company-complete)
	 :map company-active-map
	 ("C-p"  .  company-select-previous)
	 ("C-n"  .  company-select-next)
	 ("Tab"  .  company-complete-common-or-cycle)
	 )
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  (setq company-idle-delay 0.05
	company-minimum-prefix-length 2
	company-selection t
	company-require-match nil)
  (setq company-backends
 	'((
	   company-capf     ; completion at point
	   company-files    ; files & directories
 	   company-keywords ; keywords
	   company-elisp    ; emacs-lisp code
	   )))
   :hook ((prog-mode-hook . global-company-mode)
 	 (org-mode-hook . glabal-company-mode)
 	 (markdown-mode-hook . global-company-mode))
 )
(use-package flycheck)

;;(use-package spacemacs-theme
;;  :defer t)
;;(use-package doom-themes
  ;; :defer t
  ;; :init (load-theme 'doom-one-light t))	
(use-package diminish
  :config (diminish 'eldoc-mode))
(use-package modus-themes
  :ensure t
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
	modus-themes-bold-constructs nil)
 
  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  (load-theme 'modus-vivendi t)
  ;; Swap between light and dark
  :bind ("<f5>" . modus-themes-toggle)
  )

(use-package doom-modeline
  :config (doom-modeline-mode)
  (setq doom-modeline-height 1)
  (setq doom-modeline-icon 'nil))

(provide 'packages)
;;; end of packages
