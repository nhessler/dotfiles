;;; nh-lang-web.el --- Web languages (HTML, CSS, JS, TS) -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Web development languages grouped together since they're often used together.
;; Includes:
;;   - HTML (and templates: ERB, HEEx handled by their lang files)
;;   - CSS/SCSS/LESS
;;   - JavaScript
;;   - TypeScript/TSX
;;
;;;; LSP Setup
;;
;; Install language servers via Homebrew:
;;   brew install typescript-language-server    # TypeScript/JavaScript
;;   brew install vscode-langservers-extracted  # HTML, CSS, JSON
;;
;;; Code:

;;;; Web Mode
;;
;; web-mode is excellent for mixed-language templates (ERB, EEx, PHP, etc.)
;; It handles embedded scripts and styles intelligently.

(use-package web-mode
  :mode
  (("\\.html\\'" . web-mode)
   ("\\.htm\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.eex\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.vue\\'" . web-mode)
   ("\\.svelte\\'" . web-mode)
   ("\\.astro\\'" . web-mode))

  :config
  ;; Indentation
  (setq web-mode-markup-indent-offset 2)    ; HTML
  (setq web-mode-css-indent-offset 2)       ; CSS
  (setq web-mode-code-indent-offset 2)      ; JS/Ruby/PHP/etc embedded

  ;; Padding inside script/style tags
  (setq web-mode-script-padding 2)
  (setq web-mode-style-padding 2)

  ;; Auto-close tags
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)

  ;; Highlight matching tags
  (setq web-mode-enable-current-element-highlight t)

  ;; Template engines
  (setq web-mode-engines-alist
        '(("erb" . "\\.erb\\'")
          ("elixir" . "\\.eex\\'"))))

;;;; CSS Mode
;;
;; Built-in mode for CSS files.
;; For SCSS/LESS, we use dedicated modes.

(use-package css-mode
  :ensure nil  ; Built-in
  :mode "\\.css\\'"
  :hook (css-mode . eglot-ensure)
  :config
  (setq css-indent-offset 2))

;;;; SCSS Mode

(use-package scss-mode
  :mode "\\.scss\\'"
  :hook (scss-mode . eglot-ensure)
  :config
  (setq scss-compile-at-save nil))  ; Don't auto-compile

;;;; JavaScript/TypeScript
;;
;; For JavaScript, we can use js-mode (built-in) or js-ts-mode (treesitter).
;; For TypeScript, typescript-ts-mode (treesitter) is preferred.
;;
;; Eglot uses typescript-language-server for both JS and TS.

;; JavaScript (built-in mode, treesitter version used if available)
(use-package js
  :ensure nil  ; Built-in
  :mode (("\\.js\\'" . js-mode)
         ("\\.mjs\\'" . js-mode)
         ("\\.cjs\\'" . js-mode))
  :hook (js-mode . eglot-ensure)
  :config
  (setq js-indent-level 2))

;; TypeScript (treesitter mode)
;; Emacs 29+ includes typescript-ts-mode
(use-package typescript-ts-mode
  :ensure nil  ; Built-in to Emacs 29+
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :hook
  ((typescript-ts-mode . eglot-ensure)
   (tsx-ts-mode . eglot-ensure))
  :config
  (setq typescript-ts-mode-indent-offset 2))

;;;; Eglot: Web Language Server Configuration

(with-eval-after-load 'eglot
  ;; typescript-language-server for JS/TS
  ;; (Eglot has built-in support, this is for reference)
  (add-to-list 'eglot-server-programs
               '((js-mode js-ts-mode typescript-ts-mode tsx-ts-mode)
                 . ("typescript-language-server" "--stdio")))

  ;; vscode-langservers-extracted for HTML/CSS
  (add-to-list 'eglot-server-programs
               '(html-mode . ("vscode-html-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((css-mode scss-mode) . ("vscode-css-language-server" "--stdio"))))

;;;; Prettier Integration (Optional)
;;
;; Format JS/TS/CSS/HTML with Prettier on save.
;; Requires: brew install prettier
;;
;; Uncomment if you want auto-formatting:
;;
;; (use-package prettier
;;   :hook ((js-mode . prettier-mode)
;;          (typescript-ts-mode . prettier-mode)
;;          (tsx-ts-mode . prettier-mode)
;;          (css-mode . prettier-mode)
;;          (scss-mode . prettier-mode)
;;          (web-mode . prettier-mode)))

(provide 'nh-lang-web)
;;; nh-lang-web.el ends here
