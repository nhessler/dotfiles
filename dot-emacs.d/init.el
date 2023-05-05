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
  :init (doom-modeline-mode 1))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(monokai-theme doom-modeline doom-themes use-package counsel command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
