;;; nh-keys.el --- Keybinding configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Global keybindings and the overall keybinding philosophy.
;;
;; MODE-SPECIFIC BINDINGS live in their respective files (language files,
;; nh-git.el, etc.), but they follow the conventions documented here.
;;
;;;; Keybinding Conventions
;;
;; We follow Emacs conventions while reserving predictable prefixes:
;;
;; PREFIX       USAGE                           EXAMPLES
;; ────────────────────────────────────────────────────────────────
;; C-c          User & package commands         (Emacs convention)
;; C-c n        Personal (nh/) commands         C-c n r → reload config
;; C-c p        Projectile                      C-c p f → find file in project
;; C-c l        LSP/Eglot                       C-c l r → rename symbol
;; C-c g        Git (Magit)                     C-c g s → magit-status
;; C-c d        Dash documentation              C-c d d → dash-at-point
;;
;; M-           Standard Emacs commands         M-x, M-w, M-y, etc.
;; s-           Super key (macOS Cmd)           s-s → save, s-z → undo
;; C-x          Emacs file/buffer commands      C-x C-f, C-x b, etc.
;; C-h          Help commands                   C-h k → describe-key
;;
;;;; Which-Key Integration
;;
;; which-key shows available keys after a prefix is pressed.
;; This makes discovering keybindings much easier.
;;
;;; Code:

;;;; Package: which-key
;;
;; which-key displays a popup showing available keybindings when you
;; pause after pressing a prefix key (like C-c).
;;
;; Incredibly useful for learning Emacs and discovering commands.

(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)      ; Show popup after 0.5s pause
  (setq which-key-idle-secondary-delay 0.1) ; Subsequent popups faster
  (setq which-key-popup-type 'side-window)  ; Show in side window
  (setq which-key-side-window-location 'bottom)
  (setq which-key-side-window-max-height 0.25) ; Max 25% of frame height

  ;; Add descriptions for our custom prefixes
  (which-key-add-key-based-replacements
    "C-c n" "nathan"
    "C-c p" "projectile"
    "C-c l" "lsp/eglot"
    "C-c g" "git"
    "C-c d" "dash/docs")

  (which-key-mode 1))

;;;; Package: general.el
;;
;; general.el provides a cleaner API for defining keybindings.
;; It's especially useful for defining leader-key style bindings
;; and for organizing related bindings together.
;;
;; We use it primarily for the `general-define-key' macro.

(use-package general
  :config
  ;; Create a definer for our personal prefix (C-c n)
  (general-create-definer nh/leader-keys
    :prefix "C-c n")

  ;; Personal commands under C-c n
  (nh/leader-keys
    "r" '(nh/reload-config :which-key "reload config")
    "e" '((lambda () (interactive) (find-file user-init-file)) :which-key "edit init.el")
    "f" '(nh/find-config-file :which-key "find config file")))

;;;; Personal Commands (C-c n prefix)
;;
;; Custom elisp functions bound under our personal prefix.

(defun nh/reload-config ()
  "Reload Emacs configuration.
This re-evaluates init.el without restarting Emacs."
  (interactive)
  (load-file user-init-file)
  (message "Config reloaded!"))

(defun nh/find-config-file ()
  "Open a config file from the Emacs config directory.
Uses completing-read for selection."
  (interactive)
  (let ((default-directory user-emacs-directory))
    (call-interactively 'find-file)))

;;;; Window Management
;;
;; Built-in windmove uses Shift+Arrow keys to move between windows.
;; ace-window provides a faster way with M-o when you have many windows.

;; Shift+Arrow to move between windows
(require 'windmove)
(windmove-default-keybindings)

;; ace-window for jumping to windows by label
(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?l))  ; Dvorak home row
  (setq aw-scope 'frame))  ; Only show windows in current frame

;; C-<tab> for other-window (common convention)
(global-set-key (kbd "C-<tab>") 'other-window)

;;;; Buffer Management
;;
;; buffer-flip provides Alt-Tab style buffer switching.
;; Hold M-<tab> and keep pressing to cycle through buffers.

(use-package buffer-flip
  :bind
  (("M-<tab>" . buffer-flip)
   :map buffer-flip-map
   ("M-<tab>" . buffer-flip-forward)
   ("M-S-<tab>" . buffer-flip-backward)
   ("M-ESC" . buffer-flip-abort))
  :config
  ;; Skip these buffers when flipping
  (setq buffer-flip-skip-patterns
        '("^\\*helm\\b"
          "^\\*swiper\\*$"
          "^\\*Messages\\*$"
          "^\\*Compile-Log\\*$")))

;;;; Super Key (macOS Command) Bindings
;;
;; These mirror common macOS shortcuts using Command (now Super) key.
;; Makes Emacs feel more native on Mac while preserving Emacs bindings.

(when (eq system-type 'darwin)
  ;; File operations
  (global-set-key (kbd "s-s") 'save-buffer)           ; Cmd-s
  (global-set-key (kbd "s-S") 'write-file)            ; Cmd-Shift-s (Save As)
  (global-set-key (kbd "s-o") 'find-file)             ; Cmd-o (Open)
  (global-set-key (kbd "s-w") 'delete-window)         ; Cmd-w (Close window)
  (global-set-key (kbd "s-W") 'delete-frame)          ; Cmd-Shift-w (Close frame)

  ;; Edit operations
  (global-set-key (kbd "s-z") 'undo)                  ; Cmd-z
  (global-set-key (kbd "s-x") 'kill-region)           ; Cmd-x (Cut)
  (global-set-key (kbd "s-c") 'kill-ring-save)        ; Cmd-c (Copy)
  (global-set-key (kbd "s-v") 'yank)                  ; Cmd-v (Paste)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)     ; Cmd-a (Select All)

  ;; Search
  (global-set-key (kbd "s-f") 'isearch-forward)       ; Cmd-f (Find)
  (global-set-key (kbd "s-g") 'isearch-repeat-forward); Cmd-g (Find Next)
  (global-set-key (kbd "s-G") 'isearch-repeat-backward)) ; Cmd-Shift-g (Find Previous)

;;;; Hydra
;;
;; Hydra creates transient keybinding menus.
;; Press a key to enter a "hydra", then subsequent keys perform actions
;; without needing to re-press the prefix.
;;
;; We define hydras inline where they're used (e.g., window management).
;; This package is loaded here so it's available everywhere.

(use-package hydra)

;;;; Text Scaling Hydra
;;
;; Quick way to zoom in/out on text.
;; C-c n z enters the hydra, then + / - / 0 to adjust.

(defhydra nh/hydra-zoom (:hint nil)
  "
Zoom: _+_/_=_ in, _-_ out, _0_ reset, _q_ quit
"
  ("+" text-scale-increase)
  ("=" text-scale-increase)
  ("-" text-scale-decrease)
  ("0" (text-scale-set 0))
  ("q" nil :exit t))

(general-define-key
 "C-c n z" '(nh/hydra-zoom/body :which-key "zoom"))

(provide 'nh-keys)
;;; nh-keys.el ends here
