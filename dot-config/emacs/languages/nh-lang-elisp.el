;;; nh-lang-elisp.el --- Emacs Lisp configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Configuration for editing Emacs Lisp itself.
;; This is the language of Emacs configuration!
;;
;; No external LSP needed - Emacs has built-in Elisp understanding.
;; We enhance it with:
;;   - Better documentation (helpful)
;;   - REPL interaction (ielm)
;;   - Debugging (edebug)
;;   - Byte-compilation feedback
;;
;;; Code:

;;;; Emacs Lisp Mode Enhancements
;;
;; emacs-lisp-mode is built-in and excellent.
;; We add some quality-of-life improvements.

(use-package emacs-lisp-mode
  :ensure nil  ; Built-in
  :mode
  (("\\.el\\'" . emacs-lisp-mode)
   ("Cask\\'" . emacs-lisp-mode))

  :hook
  ;; Show function signatures in echo area
  (emacs-lisp-mode . eldoc-mode)
  ;; Highlight defined symbols
  (emacs-lisp-mode . highlight-defined-mode)

  :config
  ;; Indentation is already perfect for Lisp
  ;; But we can customize a few things:

  ;; When pressing RET, auto-indent the new line
  (define-key emacs-lisp-mode-map (kbd "RET") 'newline-and-indent))

;;;; Highlight Defined Symbols
;;
;; Highlights symbols that are defined (functions, variables, faces).
;; Makes it easy to spot typos - undefined symbols won't be highlighted.

(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))

;;;; Macrostep - Macro Expansion
;;
;; Interactively expand macros to see what they produce.
;; Essential for understanding and debugging macros.
;;
;; Usage: C-c m e on a macro call to expand it inline

(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c m e" . macrostep-expand)))

;;;; Elisp Demos
;;
;; Shows example usages of functions in the *Help* buffer.
;; Very handy when learning what a function does.

(use-package elisp-demos
  :config
  ;; Add demos to helpful (if installed) or standard help
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;;;; Interactive Elisp (IELM)
;;
;; A REPL for Emacs Lisp. Great for experimenting.
;; M-x ielm to start.

(use-package ielm
  :ensure nil  ; Built-in
  :config
  ;; Show eldoc in IELM too
  (add-hook 'ielm-mode-hook #'eldoc-mode))

;;;; Evaluation Helpers
;;
;; Quick ways to evaluate Elisp code.

(defun nh/eval-and-replace ()
  "Evaluate the sexp before point and replace it with the result."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (backward-kill-sexp)
    (insert (format "%S" value))))

(defun nh/eval-buffer-and-report ()
  "Evaluate the entire buffer and report success."
  (interactive)
  (eval-buffer)
  (message "Buffer evaluated: %s" (buffer-name)))

;;;; Keybindings
;;
;; Elisp-specific bindings under C-c m (mode-specific).
;;
;; Built-in evaluation commands:
;;   C-x C-e   Evaluate sexp before point
;;   C-M-x     Evaluate top-level form (defun)
;;   C-c C-e   Evaluate and print (shows result in buffer)
;;
;; Our additions:

(with-eval-after-load 'elisp-mode
  ;; Evaluation
  (define-key emacs-lisp-mode-map (kbd "C-c m b") #'nh/eval-buffer-and-report)
  (define-key emacs-lisp-mode-map (kbd "C-c m r") #'nh/eval-and-replace)

  ;; Navigation
  (define-key emacs-lisp-mode-map (kbd "C-c m d") #'xref-find-definitions)
  (define-key emacs-lisp-mode-map (kbd "C-c m D") #'xref-find-references)

  ;; Compilation
  (define-key emacs-lisp-mode-map (kbd "C-c m c") #'emacs-lisp-byte-compile)
  (define-key emacs-lisp-mode-map (kbd "C-c m l") #'emacs-lisp-byte-compile-and-load)

  ;; Documentation
  (define-key emacs-lisp-mode-map (kbd "C-c m h") #'helpful-at-point))

;;;; Lisp Interaction Mode
;;
;; For *scratch* buffer and other interactive Lisp buffers.

(use-package lisp-interaction-mode
  :ensure nil  ; Built-in
  :hook (lisp-interaction-mode . eldoc-mode))

;;;; Smartparens for Lisp
;;
;; Structural editing for S-expressions.
;; Makes balancing parens automatic.

(with-eval-after-load 'smartparens
  ;; Enable strict mode in Elisp - can't accidentally unbalance parens
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  (add-hook 'lisp-interaction-mode-hook #'smartparens-strict-mode))

;;;; Quick Reference: Elisp Development
;;
;; Evaluation:
;;   C-x C-e       Eval sexp before point (result in echo area)
;;   C-M-x         Eval defun (function/variable at point)
;;   C-c m b       Eval entire buffer
;;   M-:           Eval expression (prompt in minibuffer)
;;
;; Navigation:
;;   M-.           Go to definition (xref)
;;   M-,           Pop back from definition
;;   C-c m d       Go to definition
;;   C-c m D       Find references
;;
;; Documentation:
;;   C-h f         Describe function
;;   C-h v         Describe variable
;;   C-c m h       Helpful at point
;;
;; Debugging:
;;   M-x edebug-defun    Instrument function for debugging
;;   M-x debug-on-entry  Break when function is called
;;   C-c m e             Expand macro (macrostep)
;;
;; REPL:
;;   M-x ielm      Start Emacs Lisp REPL

(provide 'nh-lang-elisp)
;;; nh-lang-elisp.el ends here
