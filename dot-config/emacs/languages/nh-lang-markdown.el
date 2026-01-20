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

;;;; Document Margins (Gutters)
;;
;; Add left/right margins for comfortable reading, like we do for org-mode.
;; Uses nh/doc-margins-mode from nh-ui.el.
;; Margins only appear when window width exceeds 80 chars.

(add-hook 'markdown-mode-hook #'nh/doc-margins-mode)

;;;; Obsidian Integration (Optional)
;;
;; If you use Obsidian, you can add obsidian.el for wiki-link navigation.
;; Obsidian uses GFM with extensions:
;;   - [[wiki links]] for internal links
;;   - ![[embeds]] for embedding content
;;   - Tags like #tag
;;
;; To enable, uncomment and adjust vault path:
;;
;; (use-package obsidian
;;   :config
;;   (obsidian-specify-path "~/path/to/your/vault")
;;   (global-obsidian-mode t)
;;   :bind (:map obsidian-mode-map
;;               ("C-c C-o" . obsidian-follow-link-at-point)
;;               ("C-c C-b" . obsidian-backlink-jump)
;;               ("C-c C-l" . obsidian-insert-link)))

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
