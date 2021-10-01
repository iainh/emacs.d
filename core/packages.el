;; Packages

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
   (package-refresh-contents))


(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
   Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (unless (package-installed-p package)
       (package-install package)))
     packages)
)

(ensure-package-installed
 'markdown-mode
 'smartparens
 'which-key
 'exec-path-from-shell
 'magit
)

(provide 'packages)
;;; end of packages
