;;; nh-lang-markdown.el --- Markdown configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Markdown editing with nice visual appearance.
;; Used for README files, notes, documentation, etc.
;;
;;; Code:

;;;; Markdown Mode
;;
;; Major mode for Markdown files.
;; GFM (GitHub Flavored Markdown) mode is used for README.md files.

(use-package markdown-mode
  :mode
  (("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)
   ("README\\.md\\'" . gfm-mode))  ; GitHub Flavored Markdown for READMEs

  :hook
  ;; Soft wrap lines for easier reading
  (markdown-mode . visual-line-mode)

  :config
  ;; Use multimarkdown for preview/export if available
  ;; Install: brew install multimarkdown
  (setq markdown-command "multimarkdown")

  ;; Indentation for lists
  (setq markdown-list-indent-width 2)

  ;; Enable fontification of code blocks
  (setq markdown-fontify-code-blocks-natively t)

  ;; Header scaling (make headers visually larger)
  (setq markdown-header-scaling t)

  ;; Hide markup characters for cleaner look
  (setq markdown-hide-markup nil))  ; Set to t if you want hidden markup

;;;; Visual Fill Column for Markdown
;;
;; Center text and wrap at a readable width, like we do for org-mode.

(defun nh/markdown-mode-visual-fill ()
  "Set up visual-fill-column for markdown-mode."
  (setq visual-fill-column-width 100)
  (setq visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(with-eval-after-load 'visual-fill-column
  (add-hook 'markdown-mode-hook #'nh/markdown-mode-visual-fill))

;;;; Obsidian Integration
;;
;; Obsidian uses GFM with extensions:
;;   - [[wiki links]] for internal links
;;   - ![[embeds]] for embedding content
;;   - Tags like #tag
;;
;; obsidian.el provides navigation and search for Obsidian vaults.

;; Only load obsidian.el if the vault directory exists
(let ((obsidian-vault "~/Library/Mobile Documents/iCloud~md~obsidian/Documents"))
  (when (file-directory-p (expand-file-name obsidian-vault))
    (use-package obsidian
      :demand t
      :config
      ;; Set your vault path
      (obsidian-specify-path obsidian-vault)

      ;; Enable obsidian-mode in markdown files within the vault
      (global-obsidian-mode t)

      :bind (:map obsidian-mode-map
                  ;; Navigate wiki links
                  ("C-c C-o" . obsidian-follow-link-at-point)
                  ("C-c C-b" . obsidian-backlink-jump)
                  ("C-c C-l" . obsidian-insert-link)))))

;;;; Keybindings Reference
;;
;; markdown-mode provides many bindings under C-c C-...
;;
;; Formatting:
;;   C-c C-s b   Bold
;;   C-c C-s i   Italic
;;   C-c C-s c   Code
;;   C-c C-s k   Insert link
;;
;; Structure:
;;   C-c C-n/p   Navigate headings
;;   C-c C-t     Create/update TOC
;;   C-c C-o     Open link at point (or obsidian-follow-link in vault)
;;
;; Preview:
;;   C-c C-c p   Preview in browser
;;   C-c C-c l   Live preview (if available)

(provide 'nh-lang-markdown)
;;; nh-lang-markdown.el ends here
