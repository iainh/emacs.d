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

(use-package exec-path-from-shell)
(use-package smartparens)
(use-package markdown-mode)
(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status))
(use-package restclient)
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
(use-package company)
(use-package company-lsp)
(use-package rustic
  :config (setq rustic-lsp-server 'rust-analyzer)
  ;; little less flash
  (setq lsp-eldoc-hook nil)
  (setq lsp-enable-symbol-highlight nil)
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


(use-package helm
 :diminish
 :init (helm-mode t)
 :bind (("M-x"     . helm-M-x)
        ("C-x C-f" . helm-find-files)
        ("C-x b"   . helm-mini)     ;; See buffers & recent files; more useful.
        ("C-x r b" . helm-filtered-bookmarks)
        ("C-x C-r" . helm-recentf)  ;; Search for recently edited files
        ("C-c i"   . helm-imenu)
        ("C-h a"   . helm-apropos)
        ;; Look at what was cut recently & paste it in.
        ("M-y" . helm-show-kill-ring)

        :map helm-map
        ;; We can list ‘actions’ on the currently selected item by C-z.
        ("C-z" . helm-select-action)
        ;; Let's keep tab-completetion anyhow.
        ("TAB"   . helm-execute-persistent-action)
        ("<tab>" . helm-execute-persistent-action)))
(use-package rg
  :config
  (global-set-key (kbd "M-s g") 'rg)
  (global-set-key (kbd "M-s d") 'rg-dwim))

(use-package helm-rg)
(use-package flycheck)
(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))
(use-package helm-lsp
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-light t))
(use-package diminish
  :config (diminish 'eldoc-mode))
(use-package doom-modeline
  :config (doom-modeline-mode)
  (setq doom-modeline-height 1)
  (setq doom-modeline-icon 'nil))
(use-package buffer-move
  :config
  (global-set-key (kbd "<C-S-up>")     'buf-move-up)
  (global-set-key (kbd "<C-S-down>")   'buf-move-down)
  (global-set-key (kbd "<C-S-left>")   'buf-move-left)
  (global-set-key (kbd "<C-S-right>")  'buf-move-right))

(provide 'packages)
;;; end of packages
