;; Microsoft Windows

;; Disable all bells
(setq ring-bell-function 'ignore)

;; Unbind Pesky Sleep Button. On windows it only minimizes the window
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; Tree sitter binaries are available for Windows so configure the
;; packages here. In the future will move to the common packages.el
;; when there are arm binaries available or we can automate the build.
(use-package tree-sitter
  :init (global-tree-sitter-mode +1)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(provide 'windows)
