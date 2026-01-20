;;; nh-ui.el --- UI, theme, and font configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Visual appearance settings: fonts, theme, modeline, and UI polish.
;;
;; Key feature: Robust font handling with fallbacks.
;; If the preferred font isn't installed, we try alternatives instead of
;; failing and breaking the rest of the config.
;;
;; Sections:
;;   - Font Configuration (with fallbacks)
;;   - Theme
;;   - Modeline
;;   - Fringe & Line Numbers
;;   - Window Chrome
;;
;;; Code:

;;;; Font Configuration
;;
;; Fonts are tricky because they can fail if not installed.
;; We define a list of preferred fonts (in order) and use the first
;; one that's available on the system.
;;
;; All fonts here are Nerd Font variants, which include icons used by
;; doom-modeline and other packages.
;;
;; To check what fonts are available, run in Emacs:
;;   (font-family-list)
;;
;; To test if a specific font exists:
;;   (find-font (font-spec :name "MesloLGM Nerd Font Mono"))

(defvar nh/font-candidates
  '("MesloLGM Nerd Font Mono"    ; Primary choice
    "MesloLGS Nerd Font Mono"    ; Alternative Meslo variant
    "FiraCode Nerd Font Mono"    ; Popular coding font
    "JetBrainsMono Nerd Font"    ; JetBrains' coding font
    "Hack Nerd Font Mono"        ; Clean, readable
    "Menlo")                     ; macOS fallback (not Nerd Font)
  "List of fonts to try, in order of preference.
The first available font will be used.")

(defvar nh/font-size 120
  "Font size in 1/10 pt. 120 = 12pt, 140 = 14pt, etc.")

(defun nh/font-available-p (font-name)
  "Check if FONT-NAME is available on the system."
  (find-font (font-spec :name font-name)))

(defun nh/first-available-font (font-list)
  "Return the first available font from FONT-LIST, or nil if none found."
  (cl-find-if #'nh/font-available-p font-list))

(defun nh/setup-fonts ()
  "Set up fonts with fallback support.
This function is safe to call even if no fonts are available -
it will just use Emacs defaults."
  (let ((chosen-font (nh/first-available-font nh/font-candidates)))
    (if chosen-font
        (progn
          (set-face-attribute 'default nil
                              :font chosen-font
                              :height nh/font-size)
          ;; Also set for fixed-pitch (monospace in mixed documents)
          (set-face-attribute 'fixed-pitch nil
                              :font chosen-font
                              :height nh/font-size)
          (message "Font set to: %s" chosen-font))
      (message "Warning: No preferred fonts found, using Emacs defaults"))))

;; Set up fonts after Emacs is fully initialized
;; Using after-init-hook ensures the GUI is ready
(add-hook 'after-init-hook #'nh/setup-fonts)

;; Also set fonts when creating new frames (for emacsclient)
(add-hook 'server-after-make-frame-hook #'nh/setup-fonts)

;;;; Variable Pitch Font (Optional)
;;
;; For modes that benefit from proportional fonts (org-mode, etc.)
;; This is optional - uncomment if you want a different font for prose.
;;
(set-face-attribute 'variable-pitch nil
                     :font "Inter"
                     :height 140)

;;;; Theme
;;
;; Using Monokai - a classic dark theme from Sublime Text era.
;; doom-themes provides an enhanced version with better org-mode support.
;;
;; Theme loading is wrapped in condition-case to prevent errors from
;; breaking the rest of the config.

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t)     ; Enable bold where appropriate
  (setq doom-themes-enable-italic t)   ; Enable italics where appropriate

  ;; Load the theme (safely)
  ;; Using 'monokai' - if you want a different doom theme, change it here
  ;; Popular options: doom-one, doom-dracula, doom-nord, doom-gruvbox
  (condition-case err
      (load-theme 'monokai t)
    (error (message "Failed to load theme: %s" err)))

  ;; Enable flashing mode-line on errors (visual bell)
  (doom-themes-visual-bell-config)

  ;; Improve org-mode's native fontification
  (doom-themes-org-config))

;;;; Modeline
;;
;; doom-modeline provides a modern, attractive modeline with icons.
;; Requires a Nerd Font (which we set up above) for icons to display.

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25)               ; Slightly taller for readability
  (setq doom-modeline-bar-width 3)             ; Width of the "active" bar
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project) ; Show path relative to project
  (setq doom-modeline-icon t)                  ; Show icons (requires Nerd Font)
  (setq doom-modeline-major-mode-icon t)       ; Show major mode icon
  (setq doom-modeline-buffer-encoding nil)     ; Hide encoding (usually UTF-8, not interesting)
  (setq doom-modeline-indent-info nil))        ; Hide indent info (tabs/spaces)

;;;; Nerd Icons
;;
;; doom-modeline and other packages use nerd-icons for file type icons.
;; This package provides the icon functions.
;;
;; First run: You may need to install the fonts:
;;   M-x nerd-icons-install-fonts

(use-package nerd-icons
  :config
  ;; If icons aren't showing, try running:
  ;; M-x nerd-icons-install-fonts
  )

;;;; Fringe & Line Numbers
;;
;; Fringe: The thin strips on the left/right edges of windows.
;; Used for git gutter, flycheck indicators, etc.
;;
;; Line numbers: Show in programming modes, hide elsewhere.

;; Set fringe width (gives some breathing room)
(set-fringe-mode 10)

;; Line numbers only in programming modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Also show column number in modeline
(column-number-mode 1)

;;;; Window Chrome
;;
;; These were also set in early-init.el, but we set them here too
;; to ensure they're applied (belt and suspenders).
;; Some of these may be redundant but won't cause issues.

(blink-cursor-mode -1)   ; Solid cursor, no blinking
(tooltip-mode -1)        ; Use minibuffer for tooltips, not popups

;;;; Line Wrapping in Programming Modes
;;
;; In code, we usually don't want lines to wrap visually.
;; Long lines should scroll horizontally instead.

(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

;;;; Document Margins (Gutters)
;;
;; For prose-heavy modes (org, markdown), add left/right margins to create
;; a centered, comfortable reading width. Only applies when window is wide
;; enough (>80 chars), otherwise uses full width.
;;
;; Usage: Call (nh/doc-margins-mode 1) in your mode hooks.

(defvar nh/doc-margin-min-width 80
  "Minimum window width before margins are applied.
Windows narrower than this get no margins.")

(defvar nh/doc-margin-percentage 0.10
  "Percentage of window width to use for each margin (left and right).
0.10 = 10% margin on each side, leaving 80% for content.")

(defun nh/calculate-doc-margins ()
  "Calculate symmetric margins as a percentage of window width.
Returns (LEFT . RIGHT) margin values, or (0 . 0) if window is narrow."
  (let* ((window-width (window-width))
         (margin (if (> window-width nh/doc-margin-min-width)
                     (floor (* window-width nh/doc-margin-percentage))
                   0)))
    (cons margin margin)))

(defun nh/apply-doc-margins ()
  "Apply document margins to current window based on width."
  (when (bound-and-true-p nh/doc-margins-mode)
    (let ((margins (nh/calculate-doc-margins)))
      (setq left-margin-width (car margins))
      (setq right-margin-width (cdr margins))
      ;; Force window to update with new margins
      (set-window-buffer (selected-window) (current-buffer)))))

(define-minor-mode nh/doc-margins-mode
  "Minor mode that adds centered margins for comfortable reading width.
Only adds margins when window width exceeds `nh/doc-target-width'."
  :init-value nil
  :lighter nil
  (if nh/doc-margins-mode
      (progn
        (nh/apply-doc-margins)
        (add-hook 'window-state-change-hook #'nh/apply-doc-margins nil t))
    ;; Disable: remove margins and hook
    (setq left-margin-width 0)
    (setq right-margin-width 0)
    (remove-hook 'window-state-change-hook #'nh/apply-doc-margins t)
    (set-window-buffer (selected-window) (current-buffer))))

;;;; Time Display
;;
;; Show time in modeline - useful if you hide the menu bar.

(use-package time
  :ensure nil  ; Built-in
  :config
  (setq display-time-day-and-date t)    ; Show day and date
  (setq display-time-24hr-format t)     ; 24-hour time
  (setq display-time-default-load-average nil)  ; Don't show load average
  (display-time-mode 1))

(provide 'nh-ui)
;;; nh-ui.el ends here
