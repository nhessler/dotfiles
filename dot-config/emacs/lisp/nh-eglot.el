;;; nh-eglot.el --- LSP and Treesitter configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Modern IDE features using built-in Emacs 29+ packages:
;;   - Eglot: Language Server Protocol (LSP) client
;;   - Treesitter: Fast, incremental parsing for syntax highlighting
;;
;;;; Eglot vs LSP-Mode
;;
;; Eglot is built into Emacs 29+. It's simpler and lighter than lsp-mode.
;; Trade-offs:
;;   - Eglot: Less config, faster startup, fewer features
;;   - lsp-mode: More features (breadcrumbs, more UI), more complex
;;
;; We use Eglot for the "Emacs way" - minimal, integrated, just works.
;;
;;;; Treesitter
;;
;; Treesitter provides fast, accurate syntax highlighting and code navigation.
;; Emacs 29+ includes built-in treesitter support.
;;
;; For each language, you need:
;;   1. A treesitter grammar (compiled .so/.dylib file)
;;   2. A *-ts-mode (e.g., ruby-ts-mode instead of ruby-mode)
;;
;; Grammars can be installed via `treesit-install-language-grammar'.
;;
;;; Code:

;;;; Eglot - Language Server Protocol Client
;;
;; Eglot connects to language servers to provide:
;;   - Completion (via completion-at-point)
;;   - Diagnostics (errors, warnings)
;;   - Go to definition
;;   - Find references
;;   - Rename symbol
;;   - Code actions (quick fixes)
;;   - Hover documentation
;;
;; Language servers are external programs. Install them separately.
;;
;; IMPORTANT: Prefer Homebrew for LSP installation!
;; When using asdf for language version management, installing LSPs via the
;; language's package manager (gem, npm, etc.) means reinstalling for each
;; version. Homebrew installs are system-wide and version-independent.
;;
;; Homebrew LSPs (preferred):
;;   gem install ruby-lsp                     # Ruby (via asdf default-gems, NOT Homebrew!)
;;   brew install elixir-ls                   # Elixir
;;   brew install gopls                       # Go
;;   brew install typescript-language-server  # TypeScript/JavaScript
;;   brew install vscode-langservers-extracted  # HTML/CSS/JSON (all-in-one)
;;   brew install lua-language-server         # Lua
;;   brew install erlang_ls                   # Erlang
;;   brew install yaml-language-server        # YAML
;;   brew install bash-language-server        # Bash
;;
;;   brew install dockerfile-language-server  # Dockerfile

(use-package eglot
  :ensure nil  ; Built-in to Emacs 29+
  :defer t     ; Don't load until needed

  ;; Eglot is started manually or via hooks in language files
  ;; We don't enable it globally - each language file decides

  :config
  ;; Shutdown language server when last buffer is killed
  (setq eglot-autoshutdown t)

  ;; Don't log every LSP message (too noisy)
  (setq eglot-events-buffer-size 0)

  ;; Show documentation in the echo area (eldoc)
  (setq eglot-extend-to-xref t)

  ;; Use minibuffer for xref results instead of separate *xref* buffer
  ;; Jumps directly if only one result, shows completing-read if multiple
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (setq xref-show-xrefs-function #'xref-show-definitions-completing-read))


;;;; Keybindings under C-c l prefix
;;
;; These are available when eglot is active in a buffer.
;; Using with-eval-after-load ensures the keymap exists before binding.

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c l a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c l r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l f") #'eglot-format)
  (define-key eglot-mode-map (kbd "C-c l F") #'eglot-format-buffer)

  ;; Navigation
  (define-key eglot-mode-map (kbd "C-c l d") #'xref-find-definitions)
  (define-key eglot-mode-map (kbd "C-c l D") #'xref-find-references)
  (define-key eglot-mode-map (kbd "C-c l i") #'eglot-find-implementation)

  ;; Documentation
  (define-key eglot-mode-map (kbd "C-c l h") #'eldoc-doc-buffer)

  ;; Diagnostics
  (define-key eglot-mode-map (kbd "C-c l e") #'flymake-show-buffer-diagnostics)
  (define-key eglot-mode-map (kbd "C-c l n") #'flymake-goto-next-error)
  (define-key eglot-mode-map (kbd "C-c l p") #'flymake-goto-prev-error)

  ;; Server management
  (define-key eglot-mode-map (kbd "C-c l R") #'eglot-reconnect)
  (define-key eglot-mode-map (kbd "C-c l S") #'eglot-shutdown))

;;;; Treesitter Setup
;;
;; Treesitter requires grammar files for each language.
;; These are compiled shared libraries (.so on Linux, .dylib on macOS).
;;
;; `treesit-auto' can automatically install grammars and remap modes.

(use-package treesit-auto
  :demand t
  :config
  ;; Automatically install missing grammars
  (setq treesit-auto-install 'prompt)  ; Ask before installing

  ;; Where to store grammar files
  ;; user-emacs-directory is ~/.config/emacs/ (XDG location)
  ;; So grammars go in ~/.config/emacs/tree-sitter/
  (setq treesit-extra-load-path
        (list (expand-file-name "tree-sitter" user-emacs-directory)))

  ;; NOTE: We don't use global-treesit-auto-mode because it rebuilds
  ;; the grammar availability list on EVERY file open, which is slow.
  ;; Instead, we build the remap list once at startup.
  ;; Run `nh/treesit-rebuild-remap` after installing new grammars.
  (setq major-mode-remap-alist
        (treesit-auto--build-major-mode-remap-alist)))

(defun nh/treesit-rebuild-remap ()
  "Rebuild the major-mode-remap-alist for treesitter modes.
Run this after installing new treesitter grammars to use them
without restarting Emacs."
  (interactive)
  (setq major-mode-remap-alist
        (treesit-auto--build-major-mode-remap-alist))
  (message "Treesitter mode remaps rebuilt. %d mappings active."
           (length major-mode-remap-alist)))

;;;; Treesitter Grammar Sources
;;
;; Define where to download grammars from.
;; This tells `treesit-install-language-grammar' where to find source code.

(setq treesit-language-source-alist
      '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        (elixir     . ("https://github.com/elixir-lang/tree-sitter-elixir"))
        (erlang     . ("https://github.com/WhatsApp/tree-sitter-erlang"))
        (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
        (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod"))
        (heex       . ("https://github.com/phoenixframework/tree-sitter-heex"))
        (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
        (lua        . ("https://github.com/tree-sitter-grammars/tree-sitter-lua"))
        (markdown   . ("https://github.com/ikatyang/tree-sitter-markdown"))
        (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby"))
        (toml       . ("https://github.com/ikatyang/tree-sitter-toml"))
        (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
        (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))))

;;;; Helper: Install All Grammars
;;
;; Run this once to install all configured grammars:
;;   M-x nh/treesit-install-all-grammars

(defun nh/treesit-install-all-grammars ()
  "Install all treesitter grammars defined in `treesit-language-source-alist'."
  (interactive)
  (dolist (grammar treesit-language-source-alist)
    (let ((lang (car grammar)))
      (message "Installing grammar: %s" lang)
      (treesit-install-language-grammar lang)))
  (message "All grammars installed!"))

;;;; Flymake - On-the-fly Diagnostics
;;
;; Flymake shows errors/warnings as you type.
;; Eglot integrates with flymake automatically.
;;
;; We configure flymake's appearance here.

(use-package flymake
  :ensure nil  ; Built-in
  :hook (prog-mode . flymake-mode)
  :config
  ;; Show diagnostics in the fringe (gutter)
  (setq flymake-fringe-indicator-position 'left-fringe)

  ;; Delay before checking (seconds)
  (setq flymake-no-changes-timeout 0.5))

;;;; Eldoc - Documentation in Echo Area
;;
;; Eldoc shows documentation for the thing at point in the echo area.
;; Eglot uses eldoc to display type info and documentation.

(use-package eldoc
  :ensure nil  ; Built-in
  :config
  ;; Don't resize echo area constantly
  (setq eldoc-echo-area-use-multiline-p nil)

  ;; Show docs faster
  (setq eldoc-idle-delay 0.25))

;;;; Quick Reference: Eglot Commands
;;
;; Starting/Stopping:
;;   M-x eglot             Start LSP for current buffer
;;   M-x eglot-shutdown    Stop LSP server
;;   M-x eglot-reconnect   Restart connection
;;
;; Navigation (with C-c l prefix):
;;   C-c l d   Go to definition
;;   C-c l D   Find references
;;   C-c l i   Find implementation
;;
;; Editing:
;;   C-c l a   Code actions (quick fixes)
;;   C-c l r   Rename symbol
;;   C-c l f   Format buffer/region
;;
;; Diagnostics:
;;   C-c l n   Next error
;;   C-c l p   Previous error
;;   C-c l e   List all errors

(provide 'nh-eglot)
;;; nh-eglot.el ends here
