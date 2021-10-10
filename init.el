(defvar root-dir (file-name-directory load-file-name)
  "The root directory.")
(defvar core-dir (expand-file-name "core" root-dir)
  "Local configuration.")

;; add directories to Emacs' `load-path`
(add-to-list 'load-path core-dir)

;; load configuration from core/
(require 'packages)
(require 'ui)
(require 'behaviour)

;; macos specific settings
(when (eq system-type 'darwin)
  (require 'macos))

;; linux specific settings
(when (eq system-type 'gnu/linux)
  (require 'linux))

;; Windows specific settings
(when (eq system-type 'windows-nt)
  (require 'windows))
