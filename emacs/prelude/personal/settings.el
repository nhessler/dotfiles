;;;
;;; Prelude Personal Settings
;;;


;; Base Settings

(cua-mode t)

(setq-default tab-width 2)

;; theme

(prelude-require-package 'monokai-theme)

(disable-theme 'zenburn)

(load-theme 'monokai t)
(setq prelude-theme 'monokai-theme)

;; extras

(prelude-require-package 'markdown-mode)
