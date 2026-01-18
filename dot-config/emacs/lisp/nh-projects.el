;;; nh-projects.el --- Project management (Projectile) -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Projectile provides project-aware commands:
;;   - Find files within a project
;;   - Search across project files
;;   - Switch between projects
;;   - Run project commands (build, test, etc.)
;;
;; A "project" is typically a directory with:
;;   - A version control root (.git, .hg, etc.)
;;   - A project marker file (.projectile, package.json, Makefile, etc.)
;;
;;;; Project Discovery
;;
;; Projectile can auto-discover projects in specified directories.
;; We use `projectile-project-search-path' to define where to look.
;;
;; IMPORTANT: After setting the search path, we must call
;; `projectile-discover-projects-in-search-path' to actually find them.
;; This was the bug in the old config - the path was set but discovery
;; wasn't triggered.
;;
;;; Code:

;;;; Projectile - Project Management
;;
;; Core project management functionality.

(use-package projectile
  :demand t  ; Load immediately
  :bind-keymap
  ;; Bind the projectile command map to C-c p
  ;; This gives us C-c p f (find file), C-c p s r (ripgrep), etc.
  ("C-c p" . projectile-command-map)

  :init
  ;; Where to find projects
  ;; Format: '(("path" . depth))
  ;; depth = how many levels deep to search for projects
  ;;   0 = the directory itself is a project
  ;;   1 = immediate subdirectories are projects
  ;;   2 = subdirectories of subdirectories (~/Projects/user/repo)
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '(("~/Projects" . 2))))

  :config
  ;; Enable projectile globally
  (projectile-mode 1)

  ;; Use ivy for projectile completions
  (setq projectile-completion-system 'ivy)

  ;; When switching projects, immediately prompt for a file
  ;; This is the common case: you switch projects to open something.
  ;; If you need to browse, C-c p D opens dired.
  ;; Options: 'find-file, 'dired, 'commander, 'vc-dir, 'magit-status
  (setq projectile-switch-project-action #'projectile-find-file)

  ;; Disable file list caching - always get fresh file listings
  ;; This avoids stale results when files are created externally
  (setq projectile-enable-caching nil)

  ;; But DO cache project roots to avoid repeated directory traversal
  ;; This speeds up operations without causing stale file issues
  (setq projectile-project-root-files-functions
        '(projectile-root-local
          projectile-root-marked
          projectile-root-top-down
          projectile-root-bottom-up
          projectile-root-top-down-recurring))
  (setq projectile-project-root-cache (make-hash-table :test 'equal))

  ;; Indexing method
  ;; 'alien = use external tools (fd, find) - faster for large projects
  ;; 'native = pure elisp - works everywhere but slower
  (setq projectile-indexing-method 'alien)

  ;; Ignore these directories when indexing
  (setq projectile-globally-ignored-directories
        '(".git"
          ".svn"
          ".hg"
          "node_modules"
          "_build"        ; Elixir
          "deps"          ; Elixir
          ".elixir_ls"    ; Elixir LSP
          "target"        ; Rust
          "_build"        ; Various
          "build"         ; Various
          "dist"          ; JavaScript
          ".cache"
          "__pycache__"))

  ;; Ignore these files when indexing
  (setq projectile-globally-ignored-files
        '(".DS_Store"
          "*.elc"
          "*.pyc"
          "*.o"
          "*.so"
          "*.beam"))      ; Erlang/Elixir compiled

  ;; THE FIX: Actually discover projects at startup
  ;; This was missing in the old config, so projects weren't auto-populated
  (when projectile-project-search-path
    (projectile-discover-projects-in-search-path)))

;;;; Counsel-Projectile - Ivy Integration
;;
;; Provides ivy-enhanced versions of projectile commands.
;; More features and nicer UI than vanilla projectile.

(use-package counsel-projectile
  :after (projectile counsel)
  :config
  (counsel-projectile-mode 1)

  ;; Disable the find-file transformer - it calls projectile-project-root
  ;; for every candidate on every keystroke, which is very slow
  (setq counsel-projectile-find-file-transformer nil)

  ;; counsel-projectile provides enhanced versions:
  ;;   counsel-projectile-find-file    (C-c p f)
  ;;   counsel-projectile-switch-project (C-c p p)
  ;;   counsel-projectile-rg           (C-c p s r) - ripgrep search
  )

;;;; Common Projectile Keybindings Reference
;;
;; These are the default bindings under C-c p (projectile-command-map):
;;
;; Project Navigation:
;;   C-c p p   Switch to a known project
;;   C-c p f   Find file in project
;;   C-c p d   Find directory in project
;;   C-c p b   Switch buffer (within project)
;;   C-c p e   Find recently opened file in project
;;
;; Search:
;;   C-c p s r   Ripgrep search in project
;;   C-c p s g   Grep search in project
;;   C-c p s s   Ag (Silver Searcher) in project
;;
;; Project Actions:
;;   C-c p !   Run shell command in project root
;;   C-c p &   Run async shell command in project root
;;   C-c p c   Compile project
;;   C-c p P   Test project
;;   C-c p u   Run project
;;
;; File Operations:
;;   C-c p a   Switch between implementation and test file
;;   C-c p i   Invalidate project cache (force re-index)
;;   C-c p k   Kill all project buffers
;;   C-c p D   Open project root in dired
;;   C-c p v   Open project root in vc (magit-status)

(provide 'nh-projects)
;;; nh-projects.el ends here
