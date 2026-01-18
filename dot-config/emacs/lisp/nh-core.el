;;; nh-core.el --- Core Emacs settings -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Fundamental Emacs settings that don't belong to any specific package.
;; These are the "sane defaults" that make Emacs behave more predictably.
;;
;; Sections:
;;   - File Handling (backups, autosave, recent files)
;;   - Editing Behavior (tabs, whitespace, selection)
;;   - Buffer Behavior (auto-revert, uniquify)
;;   - Minibuffer & Prompts
;;   - macOS Specific
;;
;;; Code:

;;;; File Handling: Backups
;;
;; By default, Emacs creates backup files (foo.txt~) in the same directory
;; as the original file. This clutters your project directories.
;;
;; Strategy: Store ALL backups in a temporary directory instead.
;; The `backup-directory-alist' maps file patterns to backup locations.
;; ".*" matches all files, so all backups go to the temp directory.

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

;;;; File Handling: Auto-Save
;;
;; Auto-save files (#foo.txt#) are created periodically while editing.
;; They help recover work if Emacs crashes before you save.
;;
;; Like backups, we move these out of project directories.
;; `auto-save-file-name-transforms' rewrites the auto-save path.
;; The 't' at the end means "flatten the directory structure".

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;;; File Handling: Lock Files
;;
;; Lock files (.#foo.txt) prevent two Emacs instances from editing
;; the same file simultaneously. They're symlinks to user@host.pid.
;;
;; These can cause issues with some build tools (they see the symlink
;; as a changed file). Disabling is usually safe for single-user setups.

(setq create-lockfiles nil)

;;;; File Handling: Recent Files
;;
;; Recentf tracks recently opened files, making them easy to re-open.
;; Useful with completion frameworks (counsel-recentf, etc.)

(use-package recentf
  :ensure nil  ; Built-in, no need to download
  :config
  (setq recentf-max-saved-items 200)    ; Remember up to 200 files
  (setq recentf-max-menu-items 15)      ; Show 15 in menu
  (recentf-mode 1))

;;;; File Handling: Save Place
;;
;; Remember cursor position in files. When you re-open a file,
;; Emacs puts the cursor where you left off.

(use-package saveplace
  :ensure nil  ; Built-in
  :config
  (save-place-mode 1))

;;;; Buffer Behavior: Auto-Revert
;;
;; When a file changes on disk (e.g., you `git pull` in another terminal),
;; Emacs doesn't automatically update the buffer. Auto-revert fixes this.
;;
;; `global-auto-revert-mode' applies to all file buffers.
;; It checks files periodically and reverts if they changed externally.

(global-auto-revert-mode 1)

;; Also auto-refresh dired (directory) buffers when files change
(setq global-auto-revert-non-file-buffers t)

;;;; Buffer Behavior: Uniquify
;;
;; When you have multiple files with the same name (e.g., two "config.js"),
;; Emacs normally names buffers "config.js" and "config.js<2>".
;;
;; Uniquify uses part of the path instead, making it easier to identify
;; which file is which.
;;
;; Available styles:
;;   - 'forward:                    "dir/subdir/file"
;;   - 'reverse:                    "file\\dir\\subdir"
;;   - 'post-forward:               "file|dir/subdir"
;;   - 'post-forward-angle-brackets: "file<dir/subdir>"
;;
;; Using angle brackets: config.js<myproject/src>

(use-package uniquify
  :ensure nil  ; Built-in
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-after-kill-buffer-p t)    ; Re-uniquify after killing buffers
  (setq uniquify-ignore-buffers-re "^\\*")) ; Don't uniquify special buffers

;;;; Editing Behavior: Tabs vs Spaces
;;
;; The eternal debate. We default to spaces (most common in modern code).
;; Individual language modes can override this.

(setq-default indent-tabs-mode nil)  ; Use spaces, not tabs
(setq-default tab-width 2)           ; A tab is 2 spaces wide

;;;; Editing Behavior: Whitespace
;;
;; Automatically clean up trailing whitespace when saving.
;; This keeps files clean and prevents noisy git diffs.

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;; Editing Behavior: Selection (CUA-ish behavior)
;;
;; Make Emacs behave more like other text editors for selection:
;; - Typing replaces selected text (instead of inserting alongside it)

(delete-selection-mode 1)

;;;; Editing Behavior: Clipboard Integration
;;
;; Make Emacs play nicely with the system clipboard.
;; What you copy in other apps is available in Emacs and vice versa.

(setq select-enable-clipboard t)   ; Use system clipboard for cut/paste
(setq select-enable-primary t)     ; Also use primary selection (X11 middle-click)

;;;; Minibuffer & Prompts
;;
;; Small quality-of-life improvements for the minibuffer.

;; Allow typing 'y' or 'n' instead of 'yes' or 'no' at prompts
(setq use-short-answers t)  ; Emacs 28+, replaces (fset 'yes-or-no-p 'y-or-n-p)

;; Allow recursive minibuffers (using M-x while already in minibuffer)
;; Useful for complex workflows, but can be confusing if you forget where you are.
(setq enable-recursive-minibuffers t)

;;;; macOS Specific
;;
;; Mac keyboard configuration for a 44-key keyboard with homerow mods.
;;
;; With homerow modifiers on a small keyboard, you need BOTH Option keys
;; to work as Meta (e.g., M-e can't be done one-handed if E is also the
;; Meta key when held).
;;
;; Layout:
;;   Option (left & right) = Meta (M-)
;;   Command = Super (s-)

(when (eq system-type 'darwin)
  ;; macOS ls doesn't support --dired flag
  (setq dired-use-ls-dired nil)

  ;; Option keys become Meta (both sides for homerow mod compatibility)
  (setq mac-option-modifier 'meta)
  (setq mac-right-option-modifier 'meta)

  ;; Command key becomes Super
  (setq mac-command-modifier 'super))

;;;; Miscellaneous
;;
;; Other sensible defaults that don't fit elsewhere.

;; Don't beep at me
(setq ring-bell-function 'ignore)

;; Show matching parentheses immediately (no delay)
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Scrolling behavior: keep cursor away from edges
(setq scroll-margin 3)              ; Start scrolling 3 lines from edge
(setq scroll-conservatively 101)    ; Scroll just enough to keep cursor on screen

;; Sentence navigation: Single space after period ends a sentence
;; (Modern convention, vs. old typewriter double-space rule)
(setq sentence-end-double-space nil)

;; Increase undo limits (default is quite conservative)
(setq undo-limit 80000000)          ; ~80MB
(setq undo-strong-limit 120000000)  ; ~120MB
(setq undo-outer-limit 360000000)   ; ~360MB

(provide 'nh-core)
;;; nh-core.el ends here
