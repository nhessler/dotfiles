;;; nh-lang-go.el --- Go language configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Go development setup with:
;;   - go-mode (or go-ts-mode with treesitter)
;;   - Eglot integration for LSP (gopls)
;;   - Format on save with gofmt/goimports
;;
;;;; LSP Setup
;;
;; Install gopls via Homebrew (recommended):
;;   brew install gopls
;;
;; This is preferred over `go install` because:
;;   - Works across all Go versions (no reinstall needed when switching)
;;   - Homebrew handles updates automatically
;;   - No GOPATH/GOBIN configuration needed
;;
;; Note: If using asdf for Go, gopls via asdf shims also works,
;; but Homebrew is simpler for system-wide installation.
;;
;;; Code:

;;;; Go Mode
;;
;; Major mode for Go source files.
;; go-ts-mode is used automatically if treesitter grammar is available.

(use-package go-mode
  :mode "\\.go\\'"

  :hook
  ;; Start Eglot automatically
  (go-mode . eglot-ensure)
  ;; Format on save using gofmt
  (before-save . gofmt-before-save)

  :config
  ;; Indentation
  ;; Go uses actual tab characters (enforced by gofmt), but we can
  ;; display them at any width. This doesn't affect the file content,
  ;; just how tabs render on screen.
  (setq-local tab-width 2)
  (setq go-ts-mode-indent-offset 2)

  ;; Use goimports instead of gofmt (also organizes imports)
  ;; Requires: brew install goimports
  (setq gofmt-command "goimports"))

;;;; Eglot: Go Language Server Configuration
;;
;; gopls should be found in PATH automatically.
;; Eglot has built-in support for Go.

(with-eval-after-load 'eglot
  ;; gopls configuration
  ;; See: https://github.com/golang/tools/blob/master/gopls/doc/settings.md
  (setq-default eglot-workspace-configuration
                '(:gopls
                  (:staticcheck t
                   :usePlaceholders t))))

;;;; Go Commands
;;
;; Helpers for running Go commands.

(defun nh/go-run ()
  "Run the current Go file."
  (interactive)
  (compile (format "go run %s" (buffer-file-name))))

(defun nh/go-test ()
  "Run go test in project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "go test ./...")))

(defun nh/go-test-file ()
  "Run go test for the current package."
  (interactive)
  (compile (format "go test -v %s"
                   (file-name-directory (buffer-file-name)))))

(defun nh/go-build ()
  "Run go build in project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "go build ./...")))

;;;; Keybindings
;;
;; Go-specific bindings.

(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "C-c m r") #'nh/go-run)
  (define-key go-mode-map (kbd "C-c m t") #'nh/go-test)
  (define-key go-mode-map (kbd "C-c m T") #'nh/go-test-file)
  (define-key go-mode-map (kbd "C-c m b") #'nh/go-build))

(provide 'nh-lang-go)
;;; nh-lang-go.el ends here
