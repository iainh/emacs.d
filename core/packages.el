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
;;(use-package restclient)	       
(use-package which-key
  :diminish)
;;(use-package treemacs)
(use-package lsp-mode)
;;(use-package lsp-treemacs
;;  :init (lsp-treemacs-sync-mode 1))
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

(use-package projectile)
(use-package ivy
  :init (ivy-mode t))

(use-package company
  :demand t
  :hook ((prog-mode-hook . global-company-mode)
         (text-mode-hook . global-company-mode)
         (org-mode-hook . global-company-mode)
         (markdown-mode-hook . global-company-mode))
  :init
   (setq company-minimum-prefix-length 3
        company-require-match 0
        company-selection-wrap-around t
        company-tooltip-limit 10
        company-show-numbers t
        company-idle-delay 0.05)
  :bind* (("M-/"	. company-complete)
          )
  ;;        ("C-j f"	. company-files)
  ;;        ("C-j s"	. company-ispell)
  ;;        ("C-j e"	. company-elisp)
  ;;        ("C-j y"	. company-yasnippet)
  ;;        ("C-j c"	. company-dabbrev-code)
  ;;        ("C-j d"	. company-dabbrev))
  ;; :bind (:map prog-mode-map
  ;;      ("C-d" . company-dabbrev-code))
  ;; :bind (:map text-mode-map
  ;;      ("C-d" . company-dabbrev))
  :bind (:map company-active-map
              ([return] . company-complete-selection))
  ;;      ("C-n"    . company-select-next)
  ;;      ("C-p"    . company-select-previous)
        ([tab]    . yas-expand)
        ("TAB"    . yas-expand)
  ;;      ("C-w"    . backward-kill-word)
        ("C-c"    . company-abort)
	;;("C-c"    . company-search-abort))
	:diminish (company-mode . " ς")
	:config
	(setq company-backends
	      '((;; generic backends
       		 company-files    ; files & directories
		 company-keywords ; keywords
		 company-elisp    ;emacs-lisp code
		 ;;company-shell    ; shell
		 company-capf     ; completion at point
;;		 company-lsp      ; LSP
		 )))
	 (global-company-mode))
 
(use-package flycheck)
;; (use-package lsp-ui
;;   :ensure
;;   :commands lsp-ui-mode
;;   :custom
;;   (lsp-ui-peek-always-show t)
;;   (lsp-ui-sideline-show-hover t)
;;   (lsp-ui-doc-enable nil))

(use-package spacemacs-theme
  :defer t
;;  :init (load-theme 'spacemacs-light t)
)
(use-package doom-themes
  :defer t
  :init (load-theme 'doom-one-light t)
  )
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
