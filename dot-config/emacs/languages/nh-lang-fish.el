;;; nh-lang-fish.el --- Fish shell configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Fish shell script editing.
;; Fish is our default shell, so this gets used often.
;;
;;;; LSP Setup
;;
;; Install fish-lsp via Homebrew:
;;   brew install fish-lsp
;;
;;; Code:

;;;; Fish Mode
;;
;; Major mode for Fish shell scripts.

(use-package fish-mode
  :mode
  (("\\.fish\\'" . fish-mode)
   ("/fish/config\\.fish\\'" . fish-mode)
   ("/fish/functions/.*\\.fish\\'" . fish-mode))

  :hook
  ;; Start Eglot automatically for Fish files
  (fish-mode . eglot-ensure)

  :config
  ;; Indentation
  (setq fish-indent-offset 2))

;;;; Helper: Reload Fish Function
;;
;; When editing a fish function, quickly reload it in your shell.

(defun nh/fish-reload-function ()
  "Reload the current fish function file.
Runs: source <current-file> in a fish shell."
  (interactive)
  (when (and buffer-file-name
             (string-match-p "\\.fish\\'" buffer-file-name))
    (shell-command (format "fish -c 'source %s'" buffer-file-name))
    (message "Reloaded: %s" (file-name-nondirectory buffer-file-name))))

;;;; Keybindings

(with-eval-after-load 'fish-mode
  (define-key fish-mode-map (kbd "C-c m r") #'nh/fish-reload-function))

;;;; Eglot: Fish Language Server Configuration

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(fish-mode . ("fish-lsp" "start"))))

(provide 'nh-lang-fish)
;;; nh-lang-fish.el ends here
