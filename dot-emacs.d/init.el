;;; Init.el --- Nathan Hessler's Customizations
;;;
;;; Commentary:
;;;
;;; Code: 
; Appearance and Theming
(setq inhibit-startup-message t) ; Don't show the splash screen

(scroll-bar-mode -1)  ; Disable visible scrollbar
(tool-bar-mode -1 )   ; Disable the toolbar
(tooltip-mode -1)     ; Disable tooltips
(set-fringe-mode 10)  ; Give some breaching room
(menu-bar-mode -1)    ; Disable the menu bar
(column-number-mode)  ; use column number mode

(add-hook 'prog-mode-hook 'display-line-numbers-mode)


(set-face-attribute 'default nil :font "MesloLGM Nerd Font Mono" :height 120)

; Package Management
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package monokai-theme
  :config
  (load-theme 'monokai t))


(use-package command-log-mode)

(use-package ivy
  :bind ("C-s" . swiper)
  :config (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-x r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(setq ivy-use-virtual-buffers t)

(use-package counsel)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 15))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

