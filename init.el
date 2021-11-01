(defvar root-dir (file-name-directory (or load-file-name byte-compile-current-file))
  "The root directory.")
(defvar core-dir (expand-file-name "core" root-dir)
  "Local configuration.")

;; add directories to Emacs' `load-path`
(add-to-list 'load-path core-dir)

;; Improves performance for some unicode fonts
(setq inhibit-compacting-font-caches t)

;; load configuration from core/
(require 'packages)
(require 'ui)
(require 'behaviour)

;; macos specific settings
(when (eq system-type 'darwin)
  (require 'macos))

;; BSD specific settings
(when (eq system-type 'berkeley-unix)
  (require 'bsd))

;; linux specific settings
(when (eq system-type 'gnu/linux)
  (require 'linux))

;; Windows specific settings
(when (eq system-type 'windows-nt)
  (require 'windows))

(provide 'init)
