;;; init.el --- Nathan Hessler's Emacs Configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This is the main entry point for Emacs configuration.
;; It loads modular configuration files from:
;;   - lisp/     Core functionality (UI, keys, completion, git, etc.)
;;   - languages/ Language-specific configurations
;;
;; The configuration is designed to be:
;;   - Modular: Each file handles one concern
;;   - Documented: Copious comments for learning
;;   - Robust: Graceful fallbacks when things are missing
;;
;; See early-init.el for settings that run before the GUI loads.
;;
;;;; Quick Start
;;
;; 1. Install LSPs via Homebrew (see nh-eglot.el for full list):
;;      brew install ruby-lsp elixir-ls gopls typescript-language-server
;;      brew install lua-language-server erlang_ls yaml-language-server
;;      brew install bash-language-server vscode-langservers-extracted
;;
;; 2. Install treesitter grammars:
;;      M-x nh/treesit-install-all-grammars
;;
;; 3. Discover projects:
;;      Projectile auto-discovers projects in ~/Projects on startup
;;
;;;; File Organization
;;
;; lisp/
;;   nh-core.el       - Fundamental settings (backups, encoding, macOS)
;;   nh-ui.el         - Visual appearance (theme, fonts, modeline)
;;   nh-keys.el       - Global keybindings and which-key
;;   nh-completion.el - Ivy/Counsel/Swiper completion
;;   nh-projects.el   - Projectile project management
;;   nh-git.el        - Magit and Forge
;;   nh-org.el        - Org-mode configuration
;;   nh-eglot.el      - LSP (Eglot) and Treesitter
;;
;; languages/
;;   nh-lang-elisp.el    - Emacs Lisp
;;   nh-lang-ruby.el     - Ruby
;;   nh-lang-elixir.el   - Elixir + HEEx
;;   nh-lang-erlang.el   - Erlang
;;   nh-lang-go.el       - Go
;;   nh-lang-lua.el      - Lua
;;   nh-lang-web.el      - HTML/CSS/JS/TS
;;   nh-lang-fish.el     - Fish shell
;;   nh-lang-markdown.el - Markdown + Obsidian
;;   nh-lang-config.el   - YAML/TOML/JSON/Dockerfile/etc.
;;
;;; Code:

;;;; Package Management
;;
;; Set up package.el and use-package.
;; use-package is built into Emacs 29+, but we configure it here.

(require 'package)

;; Package repositories
;; MELPA has the most packages, ELPA is official GNU
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Initialize packages
(package-initialize)

;; Refresh package contents if needed (first run)
(unless package-archive-contents
  (package-refresh-contents))

;; use-package configuration
;; use-package is built into Emacs 29+
(require 'use-package)

;; Always install packages if not present
(setq use-package-always-ensure t)

;; Show loading times in *Messages* (useful for debugging slow startup)
;; Set to t to see timing info
(setq use-package-verbose nil)

;;;; Load Paths
;;
;; Add our configuration directories to the load path.
;; This lets us `require` our modules by name.

(defconst nh/lisp-dir
  (expand-file-name "lisp" user-emacs-directory)
  "Directory for core configuration modules.")

(defconst nh/languages-dir
  (expand-file-name "languages" user-emacs-directory)
  "Directory for language-specific configurations.")

(add-to-list 'load-path nh/lisp-dir)
(add-to-list 'load-path nh/languages-dir)

;;;; Load Core Modules
;;
;; These provide fundamental functionality.
;; Order matters for some of these (e.g., core before UI).

(require 'nh-core)       ; Fundamental settings
(require 'nh-ui)         ; Visual appearance
(require 'nh-keys)       ; Keybindings
(require 'nh-completion) ; Ivy/Counsel/Swiper
(require 'nh-projects)   ; Projectile
(require 'nh-git)        ; Magit/Forge
(require 'nh-org)        ; Org-mode
(require 'nh-eglot)      ; LSP and Treesitter

;;;; Load Language Modules
;;
;; Each language file configures mode, LSP, and helpers.
;; These are loaded eagerly to set up auto-mode-alist correctly.

(require 'nh-lang-elisp)    ; Emacs Lisp (for editing this config!)
(require 'nh-lang-ruby)     ; Ruby
(require 'nh-lang-elixir)   ; Elixir + HEEx
(require 'nh-lang-erlang)   ; Erlang
(require 'nh-lang-go)       ; Go
(require 'nh-lang-lua)      ; Lua
(require 'nh-lang-web)      ; HTML/CSS/JS/TS
(require 'nh-lang-fish)     ; Fish shell
(require 'nh-lang-markdown) ; Markdown + Obsidian
(require 'nh-lang-config)   ; Config files (YAML, TOML, JSON, etc.)

;;;; Custom File
;;
;; Emacs writes customizations (from M-x customize) to init.el by default.
;; We redirect them to a separate file to keep init.el clean.

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;;; Startup Complete
;;
;; Show startup time in the *Messages* buffer.

(defun nh/display-startup-time ()
  "Display the time it took to start Emacs."
  (message "Emacs ready in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'nh/display-startup-time)

;;; init.el ends here
