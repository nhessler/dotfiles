;;; nh-org.el --- Org-mode configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Org-mode is Emacs' killer feature - an outliner, note-taking system,
;; TODO manager, literate programming environment, and more.
;;
;; This config focuses on:
;;   - Clean visual appearance (superstar bullets, visual-fill-column)
;;   - Babel for code blocks (execute code in documents)
;;   - Structure templates for quick block insertion
;;
;; Key org-mode concepts:
;;   - Headlines: Lines starting with * (more stars = deeper nesting)
;;   - TODO items: Headlines with TODO/DONE keywords
;;   - Code blocks: #+begin_src ... #+end_src
;;   - Links: [[url][description]]
;;
;;; Code:

;;;; Org-mode Core Configuration

(use-package org
  :ensure nil  ; Built-in (we want the bundled version, not from ELPA)
  :hook
  ;; Enable visual line mode (soft wrap) in org buffers
  (org-mode . visual-line-mode)

  :config
  ;; Appearance
  (setq org-ellipsis " ▾")           ; Collapse indicator (instead of ...)
  (setq org-hide-emphasis-markers t)  ; Hide *bold* markers, show bold text
  (setq org-pretty-entities t)        ; Show UTF-8 chars for \alpha, etc.

  ;; Indentation
  ;; org-indent-mode visually indents content under headlines
  (setq org-startup-indented t)       ; Enable org-indent-mode by default
  (setq org-indent-indentation-per-level 2)

  ;; Editing behavior
  (setq org-return-follows-link t)    ; RET follows links
  (setq org-catch-invisible-edits 'show-and-error) ; Prevent editing folded text

  ;; Source code blocks
  (setq org-src-fontify-natively t)   ; Syntax highlight code blocks
  (setq org-src-tab-acts-natively t)  ; TAB works like in the language's mode
  (setq org-src-preserve-indentation t) ; Don't add extra indentation
  (setq org-edit-src-content-indentation 0) ; No extra indent in edit buffer

  ;; When editing src blocks, open in current window (not split)
  (setq org-src-window-setup 'current-window)

  ;;;; Babel - Execute Code Blocks
  ;;
  ;; Babel lets you run code blocks and see results inline.
  ;; Example:
  ;;   #+begin_src ruby
  ;;   (1..5).map { |n| n * 2 }
  ;;   #+end_src
  ;;
  ;; Press C-c C-c on the block to execute it.

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (ruby . t)
     (shell . t)))

  ;; Don't ask for confirmation before executing code blocks
  ;; (Remove this if you open untrusted org files)
  (setq org-confirm-babel-evaluate nil)

  ;;;; Structure Templates
  ;;
  ;; Type "<el" then TAB to expand to an emacs-lisp source block.
  ;; This uses org-tempo (built-in since Org 9.2).
  ;;
  ;; Built-in templates: <s (src), <e (example), <q (quote), <c (center)
  ;; We add custom ones below.

  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("rb" . "src ruby"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("ex" . "src elixir"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("js" . "src javascript")))

;;;; Org-Superstar - Pretty Bullets
;;
;; Replaces the asterisks (*) in headlines with nicer Unicode bullets.
;; Makes org documents much more visually appealing.

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  ;; Use simple bullets instead of fancy ones (cleaner look)
  ;; These are geometric shapes from Unicode
  (setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))

  ;; Hide the leading stars entirely
  (setq org-superstar-leading-bullet ?\s)  ; space character

  ;; Make TODO items stand out
  (setq org-superstar-special-todo-items t))

;;;; Visual-Fill-Column - Centered, Wrapped Text
;;
;; For prose-heavy org documents, it's nice to:
;;   1. Wrap text at a readable width (not full window width)
;;   2. Center the text block in the window
;;
;; This creates a "document" feel rather than a code editor feel.

(defun nh/org-mode-visual-fill ()
  "Set up visual-fill-column for org-mode.
Centers text and sets a comfortable reading width."
  (setq visual-fill-column-width 100)          ; Wrap at 100 chars
  (setq visual-fill-column-center-text t)      ; Center the text block
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . nh/org-mode-visual-fill))

;;;; Common Org-Mode Keybindings
;;
;; These are the default bindings - included here for reference.
;;
;; NAVIGATION:
;;   TAB         Cycle visibility (fold/unfold)
;;   S-TAB       Cycle visibility globally
;;   C-c C-n/p   Next/previous heading
;;   C-c C-f/b   Next/previous heading (same level)
;;   C-c C-u     Up to parent heading
;;
;; STRUCTURE EDITING:
;;   M-RET       Insert new heading/item
;;   M-S-RET     Insert new TODO heading
;;   M-LEFT/RIGHT   Promote/demote heading
;;   M-UP/DOWN      Move subtree up/down
;;
;; TODO:
;;   C-c C-t     Rotate TODO state
;;   S-LEFT/RIGHT   Rotate TODO state
;;
;; LINKS:
;;   C-c C-l     Insert/edit link
;;   C-c C-o     Open link at point
;;   RET         Open link (with our config)
;;
;; CODE BLOCKS:
;;   C-c C-c     Execute code block
;;   C-c '       Edit code block in dedicated buffer

(provide 'nh-org)
;;; nh-org.el ends here
