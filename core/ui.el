;; UI Customization

;; Cursor configuration
(setq-default cursor-type 'box)

;; Use the modus theme magenta-alt colour for the cursor colour via a hook so that it is
;; applied after theme change.
(defun my-modus-themes-custom-faces ()
  (set-face-attribute 'cursor nil :background (modus-themes-color 'magenta-alt))
  (set-face-attribute 'font-lock-type-face nil :foreground (modus-themes-color 'magenta-alt)))

(add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-custom-faces)

;; Theme setting. Attempt to do the right thing when a window is not present
;; and sent a dark theme to fit it with my dark terminal sessions.
(if (display-graphic-p)
    (load-theme 'modus-operandi t)
  (load-theme 'modus-vivendi t))

(when (display-graphic-p)
  ;; More useful frame title, that show either a file or a buffer name
  (setq frame-title-format
	'("" "Emacs " emacs-version " " (:eval (if (buffer-file-name)
						   (abbreviate-file-name (buffer-file-name))
						 "%b"))))

  ;; Font customization. If 'Fira Code' is available, use it and setup all
  ;; customizations for the variants. If not available, use 'monospace'
  (if (member "IBM Plex Mono" (font-family-list))
      (progn
	(custom-set-faces
	 '(default ((t (:height 175 :family "SomeType Mono"))))
	 ;; Use a less bold variant of Fira Code (disabled due to issue on macos)
	 ;; '(bold ((t (:family "Fira Code SemiBold"))))
	 '(mode-line ((t (:height 110))))
	 '(mode-line-inactive ((t (:height 110))))))
    (set-face-attribute 'default nil :font "monospace" :height 140))

  ;; semibold is the new bold
  (set-face-attribute 'bold nil :weight 'semibold)

  ;; Line spacing
  (setq-default line-spacing-vertical-center 1)
  (setq-default line-spacing 0.2)

  (use-package prog-mode
    :straight (:type built-in)
    :hook(;; Show line numbers at the beginning of each line
	  (prog-mode . display-line-numbers-mode)
	  ;; Hightlight the current line
	  (prog-mode . hl-line-mode)))
 

  ;; End of graphics only block
  )

(provide 'ui)
;;; ui.el ends here
