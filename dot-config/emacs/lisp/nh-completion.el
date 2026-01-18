;;; nh-completion.el --- Completion framework (Ivy/Counsel) -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Completion in Emacs refers to the system that helps you:
;;   - Complete commands (M-x)
;;   - Complete file names (C-x C-f)
;;   - Complete buffer names (C-x b)
;;   - Search text (swiper)
;;   - And much more...
;;
;; We use Ivy + Counsel + Swiper:
;;   - Ivy:     The core completion engine (minibuffer completion UI)
;;   - Counsel: Ivy-enhanced versions of common commands (M-x, find-file, etc.)
;;   - Swiper:  Ivy-based search (replaces isearch for in-buffer search)
;;
;; Alternative: Vertico + Consult + Orderless
;;   If you want to try the "newer" approach later, replace this file.
;;   Vertico uses Emacs' native completing-read, while Ivy has its own system.
;;
;;; Code:

;;;; Ivy - The Core Completion Engine
;;
;; Ivy provides a completing-read replacement with a nice minibuffer UI.
;; It shows candidates in a vertical list and supports fuzzy matching.

(use-package ivy
  :demand t  ; Load immediately (not deferred)
  :config
  ;; Ivy appearance
  (setq ivy-height 15)               ; Show 15 candidates
  (setq ivy-count-format "(%d/%d) ") ; Show match count
  (setq ivy-use-virtual-buffers t)   ; Include recent files in buffer switch
  (setq ivy-wrap t)                  ; Wrap around when navigating

  ;; Don't start searches with ^ (anchor to beginning)
  ;; This makes searches more flexible/fuzzy by default
  (setq ivy-initial-inputs-alist nil)

  ;; Use different regex styles per command
  ;; 'ivy--regex-plus: spaces mean .* (fuzzy-ish)
  ;; 'ivy--regex-ignore-order: match terms in any order
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
          (t . ivy--regex-plus)))

  (ivy-mode 1))

;;;; Ivy-Rich - Enhanced Ivy Display
;;
;; ivy-rich adds extra information to ivy candidates:
;;   - File sizes and modification times
;;   - Function documentation in M-x
;;   - More context to help you choose

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1))

;;;; Counsel - Ivy-Enhanced Commands
;;
;; Counsel replaces common Emacs commands with ivy-powered versions.
;; These provide better previews, more candidates, and nicer UI.

(use-package counsel
  :after ivy
  :demand t
  :bind
  (;; Replace standard commands with counsel versions
   ("M-x" . counsel-M-x)              ; Enhanced M-x with keybinding hints
   ("C-x C-f" . counsel-find-file)    ; Enhanced file finder
   ("C-x b" . counsel-switch-buffer)  ; Enhanced buffer switch
   ("C-x C-b" . counsel-ibuffer)      ; Enhanced buffer list

   ;; Help commands
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-h o" . counsel-describe-symbol)

   ;; Search commands
   ("C-c s r" . counsel-rg)           ; Ripgrep search in project
   ("C-c s g" . counsel-git-grep)     ; Git grep

   ;; Yank (paste) from kill ring with preview
   ("M-y" . counsel-yank-pop)

   ;; Minibuffer history navigation
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history))

  :config
  ;; When finding files, ignore these patterns
  ;; NOTE: We don't hide dotfiles because .tool-versions, .env, etc. are important!
  (setq counsel-find-file-ignore-regexp
        (concat
         "\\(?:[#~]\\'\\)"           ; Backup files ending in # or ~
         "\\|\\(?:\\`#\\)"           ; Autosave files starting with #
         "\\|__pycache__"            ; Python cache
         "\\|node_modules"           ; Node modules
         "\\|\\.elc\\'")))           ; Compiled elisp

;;;; Swiper - Ivy-Based Search
;;
;; Swiper replaces isearch (C-s) with an ivy-based search.
;; Shows all matches in a list and lets you jump between them.
;;
;; Super useful for:
;;   - Seeing all matches at once
;;   - Navigating by content rather than position
;;   - Searching with ivy's fuzzy matching

(use-package swiper
  :bind
  (("C-s" . swiper)                   ; Replace isearch-forward
   ("C-r" . swiper-backward)))        ; Replace isearch-backward

;;;; Helpful - Better Help Buffers
;;
;; Replaces Emacs' *Help* buffers with much more informative versions.
;; Shows source code, references, and more.
;;
;; Integrates with counsel's describe commands.

(use-package helpful
  :custom
  ;; Tell counsel to use helpful for function/variable descriptions
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ;; Remap standard help commands to helpful versions
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol))

(provide 'nh-completion)
;;; nh-completion.el ends here
