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
(use-package magit)
(use-package restclient)
(use-package which-key)
(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-light t))

(provide 'packages)
;;; end of packages
