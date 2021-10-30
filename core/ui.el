;; UI Customization
(setq inhibit-startup-screen t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Cursor configuration
(setq-default cursor-type '(bar . 2))

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
  (if (member "Inconsolata" (font-family-list))
      (progn
	(custom-set-faces
	 '(default ((t (:height 190 :family "Inconsolata"))))
	 ;; Use a less bold variant of Fira Code (disabled due to issue on macos)
	   ;; '(bold ((t (:family "Fira Code SemiBold"))))
	 '(mode-line ((t (:height 110))))
	 '(mode-line-inactive ((t (:height 110))))))
    (set-face-attribute 'default nil :font "monospace" :height 140))

  ;; A hack to vertically centre the text on a line until emacs supports the
  ;; true centering. Patch by Jesse Medeiros started 2019:
  ;;   https://yhetil.org/emacs-devel/87eeewak2c.fsf@gnus.org/T/#u
  (defun set-bigger-spacing ()
    (setq-local default-text-properties '(line-spacing 0.125 line-height 1.125)))

  (use-package text-mode
    :straight (:type built-in)
    :hook (text-mode . set-bigger-spacing))

  (use-package prog-mode
    :straight (:type built-in)
    :hook(;; Show line numbers at the beginning of each line
	  (prog-mode . display-line-numbers-mode)
	  ;; Hightlight the current line
	  (prog-mode . hl-line-mode)
	  (prog-mode . set-bigger-spacing)))

  ;; Ligatures
  (let ((alist '((?! . "\\(?:!\\(?:==\\|[!=]\\)\\)")
		 (?# . "\\(?:#\\(?:###?\\|_(\\|[!#(:=?[_{]\\)\\)")
		 (?$ . "\\(?:\\$>\\)")
		 (?& . "\\(?:&&&?\\)")
		 (?* . "\\(?:\\*\\(?:\\*\\*\\|[/>]\\)\\)")
		 (?+ . "\\(?:\\+\\(?:\\+\\+\\|[+>]\\)\\)")
		 (?- . "\\(?:-\\(?:-[>-]\\|<<\\|>>\\|[<>|~-]\\)\\)")
		 (?. . "\\(?:\\.\\(?:\\.[.<]\\|[.=?-]\\)\\)")
		 (?/ . "\\(?:/\\(?:\\*\\*\\|//\\|==\\|[*/=>]\\)\\)")
		 (?: . "\\(?::\\(?:::\\|\\?>\\|[:<-?]\\)\\)")
		 (?\; . "\\(?:;;\\)")
		 (?< . "\\(?:<\\(?:!--\\|\\$>\\|\\*>\\|\\+>\\|-[<>|]\\|/>\\|<[<=-]\\|=\\(?:=>\\|[<=>|]\\)\\||\\(?:||::=\\|[>|]\\)\\|~[>~]\\|[$*+/:<=>|~-]\\)\\)")
		 (?= . "\\(?:=\\(?:!=\\|/=\\|:=\\|=[=>]\\|>>\\|[=>]\\)\\)")
		 (?> . "\\(?:>\\(?:=>\\|>[=>-]\\|[]:=-]\\)\\)")
		 (?? . "\\(?:\\?[.:=?]\\)")
		 (?\[ . "\\(?:\\[\\(?:||]\\|[<|]\\)\\)")
		 (?\ . "\\(?:\\\\/?\\)")
		 (?\] . "\\(?:]#\\)")
		 (?^ . "\\(?:\\^=\\)")
		 (?_ . "\\(?:_\\(?:|?_\\)\\)")
		 (?{ . "\\(?:{|\\)")
		 (?| . "\\(?:|\\(?:->\\|=>\\||\\(?:|>\\|[=>-]\\)\\|[]=>|}-]\\)\\)")
		 (?~ . "\\(?:~\\(?:~>\\|[=>@~-]\\)\\)"))))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring]))))

  ;; End of graphics only block
  )


(provide 'ui)
;;; ui.el ends here
