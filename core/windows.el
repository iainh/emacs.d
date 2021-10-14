;; Microsoft Windows

;; Disable all bells
(setq ring-bell-function 'ignore)

;; Unbind Pesky Sleep Button. On windows it only minimizes the window
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

(provide 'windows)
