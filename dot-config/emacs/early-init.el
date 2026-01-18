;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This file runs BEFORE init.el and BEFORE the GUI frame is created.
;; It's the right place for:
;;   - Disabling UI elements (prevents flicker on startup)
;;   - Performance tweaks that must happen early
;;   - Package.el configuration
;;
;; Emacs loads files in this order:
;;   1. early-init.el (this file)
;;   2. Frame is created
;;   3. init.el
;;
;; Because the frame doesn't exist yet, we can disable menu-bar, tool-bar,
;; etc. BEFORE they're ever drawn. This is faster and cleaner than disabling
;; them after they appear (which causes a visible flicker).
;;
;;; Code:

;;;; Performance: Garbage Collection
;;
;; Garbage collection (GC) pauses Emacs to clean up unused memory.
;; During startup, lots of objects are created, triggering many GC runs.
;;
;; Strategy: Set GC threshold very high during startup (don't collect),
;; then reset to a reasonable value after init completes (see init.el).
;;
;; `most-positive-fixnum' is the largest integer Emacs can represent.
;; This effectively disables GC during startup.

(setq gc-cons-threshold most-positive-fixnum)

;;;; Package.el Configuration
;;
;; By default, Emacs initializes the package system before loading init.el.
;; We want to control this ourselves (in init.el) so we can set up
;; package archives and use-package before anything loads.

(setq package-enable-at-startup nil)

;;;; UI Elements: Disable Before Frame Creation
;;
;; These settings disable UI elements BEFORE the frame exists.
;; Using `default-frame-alist' affects all future frames.
;; This is faster than calling (menu-bar-mode -1) etc. in init.el.

;; Disable the startup screen and messages
(setq inhibit-startup-message t)   ; No "Welcome to GNU Emacs" buffer
(setq inhibit-startup-screen t)    ; Same thing (alias)

;; Configure the *scratch* buffer
;; This buffer exists when Emacs starts - we'll use it as a markdown scratchpad
(setq initial-scratch-message nil) ; No default message in scratch buffer
(setq initial-major-mode 'markdown-mode) ; Use markdown instead of lisp-interaction

;; Disable UI chrome via frame parameters (before frame is created)
;; Format: (push '(PARAMETER . VALUE) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)    ; No menu bar
(push '(tool-bar-lines . 0) default-frame-alist)    ; No tool bar
(push '(vertical-scroll-bars) default-frame-alist)  ; No scroll bars

;;;; Performance: Frame Resizing
;;
;; When Emacs changes fonts or UI elements, it may resize the frame.
;; This is slow and visually jarring. Disable it.

(setq frame-inhibit-implied-resize t)

;;;; Performance: Bidirectional Text
;;
;; Emacs supports bidirectional text (mixing left-to-right and right-to-left
;; scripts like English and Hebrew). This requires extra computation during
;; display to determine text direction.
;;
;; If you only use left-to-right languages, disabling this saves CPU cycles.
;; Note: Files with RTL text would display incorrectly with this disabled.

(setq-default bidi-display-reordering nil)

(provide 'early-init)
;;; early-init.el ends here
