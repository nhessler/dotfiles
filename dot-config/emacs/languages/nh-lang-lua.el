;;; nh-lang-lua.el --- Lua language configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Lua development setup with:
;;   - lua-mode (or lua-ts-mode with treesitter)
;;   - Eglot integration for LSP (lua-language-server)
;;
;; Lua is used in many contexts:
;;   - Neovim configuration
;;   - Game development (Love2D, Corona, Defold)
;;   - Embedded scripting (Redis, nginx, Hammerspoon)
;;   - Configuration files (Awesome WM, etc.)
;;
;;;; LSP Setup
;;
;; Install lua-language-server via Homebrew:
;;   brew install lua-language-server
;;
;;; Code:

;;;; Lua Mode
;;
;; Major mode for editing Lua source files.
;; Provides syntax highlighting, indentation, and basic commands.

(use-package lua-mode
  :mode "\\.lua\\'"

  :hook
  ;; Start Eglot automatically for Lua files
  (lua-mode . eglot-ensure)

  :config
  ;; Indentation - 2 spaces is common in Lua
  (setq lua-indent-level 2)

  ;; Don't add extra indentation for nested blocks
  (setq lua-indent-nested-block-content-align nil)

  ;; String continuation - don't indent continuation lines extra
  (setq lua-indent-string-contents nil))

;;;; Eglot: Lua Language Server Configuration
;;
;; lua-language-server (sumneko) provides excellent Lua support.
;; Homebrew installs it as 'lua-language-server'.

(with-eval-after-load 'eglot
  ;; lua-language-server should be found in PATH via Homebrew
  (add-to-list 'eglot-server-programs
               '(lua-mode . ("lua-language-server"))))

;;;; Lua Commands
;;
;; Helpers for running Lua code.

(defun nh/lua-run-file ()
  "Run the current Lua file."
  (interactive)
  (compile (format "lua %s" (buffer-file-name))))

(defun nh/lua-run-love ()
  "Run Love2D game from project root.
Looks for main.lua in the project directory."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "love .")))

;;;; Keybindings
;;
;; Lua-specific bindings.

(with-eval-after-load 'lua-mode
  (define-key lua-mode-map (kbd "C-c m r") #'nh/lua-run-file)
  (define-key lua-mode-map (kbd "C-c m l") #'nh/lua-run-love))

;;;; Fennel Support (Optional)
;;
;; Fennel is a Lisp that compiles to Lua.
;; Used in some game development and Neovim configs.
;;
;; Uncomment if you use Fennel:
;;
;; (use-package fennel-mode
;;   :mode "\\.fnl\\'"
;;   :config
;;   (setq fennel-mode-switch-to-repl-after-reload nil))

(provide 'nh-lang-lua)
;;; nh-lang-lua.el ends here
