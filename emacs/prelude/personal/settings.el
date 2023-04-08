;;;
;;; Prelude Personal Settings
;;;


;; Base Settings

(cua-mode t)

;; Only spaces, no tabs
(setq indent-tabs-mode nil)

;; Always end a file with a newline
(setq require-final-newline nil)

(setq-default tab-width 2)
(setq-default web-mode-markup-indent-offset 2)


;; theme

(prelude-require-package 'monokai-theme)

(disable-theme 'zenburn)

(load-theme 'monokai t)
(setq prelude-theme 'monokai-theme)

;; extras

(prelude-require-package 'markdown-mode)
