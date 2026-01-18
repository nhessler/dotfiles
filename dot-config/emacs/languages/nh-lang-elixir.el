;;; nh-lang-elixir.el --- Elixir language configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Elixir development setup with:
;;   - elixir-mode (or elixir-ts-mode with treesitter)
;;   - Eglot integration for LSP (elixir-ls or next-ls)
;;   - HEEx template support
;;   - Mix integration
;;
;;;; LSP Setup
;;
;; Install elixir-ls via Homebrew (recommended):
;;   brew install elixir-ls
;;
;; This is preferred over asdf because:
;;   - Works across all Elixir versions (no reinstall needed)
;;   - Homebrew handles updates automatically
;;   - No build issues with specific OTP versions
;;
;; Alternative: next-ls (newer, still maturing):
;;   Download from https://github.com/elixir-tools/next-ls/releases
;;
;;; Code:

;;;; Elixir Mode
;;
;; Major mode for editing Elixir source files.
;; Provides syntax highlighting, indentation, and basic commands.

(use-package elixir-mode
  :mode
  (("\\.ex\\'" . elixir-mode)
   ("\\.exs\\'" . elixir-mode)
   ("mix\\.lock\\'" . elixir-mode))

  :hook
  ;; Start Eglot automatically for Elixir files
  ;; Include both modes since treesit-auto may use elixir-ts-mode
  ((elixir-mode elixir-ts-mode) . eglot-ensure)

  :bind (:map elixir-mode-map
              ;; Format buffer with mix format
              ("C-c C-f" . elixir-format))

  :config
  ;; Indentation
  (setq elixir-smie-indent-basic 2))

;;;; HEEx Templates
;;
;; HEEx is Phoenix's HTML template format (replaces EEx in LiveView).
;; Files: .heex
;;
;; heex-ts-mode uses treesitter for better parsing.
;; Falls back to web-mode if treesitter grammar isn't available.

(use-package heex-ts-mode
  :mode "\\.heex\\'"
  :hook (heex-ts-mode . eglot-ensure))

;;;; Eglot: Elixir Language Server Configuration
;;
;; Tell Eglot how to find and start elixir-ls.
;; Adjust the path if you installed it elsewhere.

(with-eval-after-load 'eglot
  ;; Option 1: elixir-ls from PATH or specific location
  ;; Looks for 'elixir-ls' in PATH, or uses the explicit path
  (let ((elixir-ls-path (or (executable-find "elixir-ls")
                            (executable-find "language_server.sh")
                            (expand-file-name "~/.local/share/elixir-ls/language_server.sh"))))
    (when elixir-ls-path
      (add-to-list 'eglot-server-programs
                   `((elixir-mode elixir-ts-mode heex-ts-mode) . (,elixir-ls-path))))))

;;;; Mix Commands
;;
;; Helpers for running Mix tasks.
;; These compile/run in a compilation buffer.

(defun nh/mix-compile ()
  "Run mix compile in project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "mix compile")))

(defun nh/mix-test ()
  "Run mix test in project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "mix test")))

(defun nh/mix-test-file ()
  "Run mix test for the current file."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile (format "mix test %s" (buffer-file-name)))))

(defun nh/mix-format ()
  "Run mix format on current file."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (shell-command (format "mix format %s" (buffer-file-name)))
    (revert-buffer t t t)))

;;;; Keybindings
;;
;; Elixir-specific bindings under the mode map.

(with-eval-after-load 'elixir-mode
  (define-key elixir-mode-map (kbd "C-c m c") #'nh/mix-compile)
  (define-key elixir-mode-map (kbd "C-c m t") #'nh/mix-test)
  (define-key elixir-mode-map (kbd "C-c m T") #'nh/mix-test-file)
  (define-key elixir-mode-map (kbd "C-c m f") #'nh/mix-format))

(provide 'nh-lang-elixir)
;;; nh-lang-elixir.el ends here
