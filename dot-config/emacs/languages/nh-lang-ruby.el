;;; nh-lang-ruby.el --- Ruby language configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Ruby development setup with:
;;   - ruby-mode (or ruby-ts-mode with treesitter)
;;   - Eglot integration for LSP (ruby-lsp)
;;   - ERB template support
;;   - Bundler/Rake integration
;;
;;;; LSP Setup
;;
;; Install ruby-lsp via Homebrew (recommended):
;;   brew install ruby-lsp
;;
;; This is preferred over `gem install` because:
;;   - Works across all Ruby versions (no reinstall needed when switching)
;;   - Homebrew handles updates automatically
;;   - No gemset/bundler conflicts
;;
;; Alternative: Add to ~/.default-gems for asdf:
;;   echo "ruby-lsp" >> ~/.default-gems
;;   (Installs automatically with each new Ruby version)
;;
;;; Code:

;;;; Ruby Mode
;;
;; Built-in major mode for Ruby files.
;; Treesitter version (ruby-ts-mode) is used automatically if grammar exists.

(use-package ruby-mode
  :ensure nil  ; Built-in
  :mode
  (("\\.rb\\'" . ruby-mode)
   ("\\.rake\\'" . ruby-mode)
   ("\\.ru\\'" . ruby-mode)
   ("\\.gemspec\\'" . ruby-mode)
   ("Gemfile\\'" . ruby-mode)
   ("Rakefile\\'" . ruby-mode)
   ("Guardfile\\'" . ruby-mode)
   ("Capfile\\'" . ruby-mode)
   ("Vagrantfile\\'" . ruby-mode)
   ("\\.builder\\'" . ruby-mode)
   ("\\.jbuilder\\'" . ruby-mode)
   ("\\.thor\\'" . ruby-mode)
   ("\\.podspec\\'" . ruby-mode))

  :hook
  ;; Start Eglot automatically for Ruby files
  (ruby-mode . eglot-ensure)

  :config
  ;; Indentation
  (setq ruby-indent-level 2)
  (setq ruby-indent-tabs-mode nil)

  ;; Don't insert encoding comment at top of file
  (setq ruby-insert-encoding-magic-comment nil)

  ;; Align method arguments nicely
  (setq ruby-align-chained-calls t)
  (setq ruby-align-to-stmt-keywords t))

;;;; ERB Templates
;;
;; ERB is Ruby's templating format embedded in HTML.
;; Files: .erb, .html.erb, .js.erb, etc.
;;
;; We use web-mode which handles ERB well.

(with-eval-after-load 'nh-lang-web
  ;; ERB files are handled by web-mode in nh-lang-web.el
  ;; This adds Ruby-specific associations
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\.erb\\'" . web-mode)))

;;;; Eglot: Ruby Language Server Configuration
;;
;; ruby-lsp is auto-detected by Eglot, but we can configure it here.

(with-eval-after-load 'eglot
  ;; ruby-lsp should be found in PATH (brew install ruby-lsp)
  ;; Eglot has built-in support, but we ensure it's configured
  (add-to-list 'eglot-server-programs
               '((ruby-mode ruby-ts-mode) . ("ruby-lsp"))))

;;;; Bundler/Rake Commands
;;
;; Helpers for running common Ruby commands.

(defun nh/bundle-install ()
  "Run bundle install in project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "bundle install")))

(defun nh/rake-test ()
  "Run rake test in project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "bundle exec rake test")))

(defun nh/rspec-run ()
  "Run RSpec for the project."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "bundle exec rspec")))

(defun nh/rspec-run-file ()
  "Run RSpec for the current file."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile (format "bundle exec rspec %s" (buffer-file-name)))))

;;;; Keybindings
;;
;; Ruby-specific bindings.

(with-eval-after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "C-c m b") #'nh/bundle-install)
  (define-key ruby-mode-map (kbd "C-c m t") #'nh/rake-test)
  (define-key ruby-mode-map (kbd "C-c m r") #'nh/rspec-run)
  (define-key ruby-mode-map (kbd "C-c m R") #'nh/rspec-run-file))

;;;; Smartparens Configuration
;;
;; Ruby has special block structures (do..end, def..end, etc.)
;; Smartparens handles these.

(with-eval-after-load 'smartparens
  (require 'smartparens-ruby))

(provide 'nh-lang-ruby)
;;; nh-lang-ruby.el ends here
