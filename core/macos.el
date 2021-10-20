;; On macOS Emacs doesn't use the shell PATH if it's not started from
;; the shell. Let's fix that:

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(when window-system
  ;; Enable menubar on macOS
  (menu-bar-mode +1)

  ;; macOS specific UI configuration
  (use-package ns-auto-titlebar
    :init (ns-auto-titlebar-mode))

  (set-fontset-font t 'symbol (font-spec :family "Apple Symbols") nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

;; macOS ls doesn't support '-X' or arguments like '--sort' so use coreutils ls
;; from macPorts
(when (equal system-type 'darwin)
  (setq dired-use-ls-dired t
	insert-directory-program "/opt/local/bin/gls"))

;; From https://github.com/emacs-tree-sitter/elisp-tree-sitter/issues/88#issuecomment-849338234
;; The intent is to compile tree sitter cli and the language bindings locally until
;; the pre built binaries support arm64 macs.
;; TODO: This configuration didn't work for me on a clean install but did help me get
;;       it working with some trial and error so I'm leaving it in until I can refine
;;       it sufficiently.
(use-package tsc
  :straight `(:pre-build ,(when (and (memq window-system '(mac ns))
                                     (string-match-p (rx string-start "arm-")
                                                     system-configuration))
                            ;; required for tree-sitter
                            (unless (and (executable-find "cargo")
                                         ;; required for building bindings
                                         (executable-find "cask")
                                         (executable-find "git")
                                         ;; required for tree-sitter to generate
                                         (executable-find "npm")
                                         ;; required for bindings
                                         (executable-find "llvm-gcc"))
                              (warn "tree-sitter build will fail"))
                            (setf lyn--self-compiled-tsc t)
                            ;; get tree-sitter v0.19.5 - last to put files in a reasonable place
                            '(("sh" "-c" "test -d rust-tree-sitter || git clone https://github.com/tree-sitter/tree-sitter rust-tree-sitter; cd rust-tree-sitter && git fetch && git checkout v0.19.5")
                              ("sh" "-c" "cd rust-tree-sitter/cli && cargo install --path .")
                              ;; needed or it will download x86_64 dylibs over the arm64 ones we just built
                              ("sh" "-c" "file core/tsc-dyn.dylib | grep -q arm64 || rm -f core/tsc-dyn.dylib")
                              ("sh" "-c" "grep -q LOCAL core/DYN-VERSION || printf LOCAL >core/DYN-VERSION")
                              ("sh" "-c" "grep -q DYN-VERSION bin/build && sed -e '/DYN-VERSION/d' bin/build >bin/build.tmp && mv bin/build.tmp bin/build && chmod +x bin/build || :")
                              ;; rebuild bindings
                              ("sh" "-c" "EMACS=emacs ./bin/setup && EMACS=emacs ./bin/build")
                              ;; ensure all language definitions
                              ("find" "langs/repos" "-type" "f" "-name" "grammar.js" "-not" "-path" "*/node_modules/*" "-not" "-path" "*/ocaml/interface/*" "-exec" "sh" "-c" "targets=''; for grammar_file in \"$@\"; do grammar_dir=\"${grammar_file%/*}\"; targets=\"$targets ensure/${grammar_dir##*/}\"; done; EMACS=emacs make -j7 $targets" "sh" "{}" "+")))
                         :files ("core/DYN-VERSION" "core/tsc-dyn.*" "core/*.el")))
(use-package tree-sitter
  :init (tree-sitter-hl-mode))

(use-package tree-sitter-langs
  ;; Don't clone the separate tree-sitter-langs repo, use the dylibs we already built
  :straight (:host github :repo "ubolonton/emacs-tree-sitter"
             :files ("langs/*.el" ("bin" "langs/bin/*.dylib") ("queries" "langs/queries/*")))
  :after tree-sitter
  ;; If this isn't set then it'll download x86_64 dylibs over the arm64 dylibs we built
  :init (setf tree-sitter-langs--testing lyn--self-compiled-tsc))

(provide 'macos)
;;; end of macos.el
