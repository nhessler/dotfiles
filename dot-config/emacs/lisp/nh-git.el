;;; nh-git.el --- Git integration (Magit, Forge) -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Git integration using Magit - widely considered the best Git interface
;; in any editor. If you're new to Magit, try this workflow:
;;
;;   1. C-c g s   (or C-x g) → Open magit-status
;;   2. Navigate to a file, press TAB to expand/collapse diff
;;   3. Stage changes: 's' on a file or hunk
;;   4. Commit: 'c c', write message, C-c C-c to confirm
;;   5. Push: 'P p'
;;
;; Forge adds GitHub/GitLab integration:
;;   - View and create issues and PRs
;;   - Review PR changes
;;   - Manage notifications
;;
;;;; Keybinding Philosophy
;;
;; Git commands are under C-c g prefix:
;;   C-c g s   magit-status (the main entry point)
;;   C-c g l   magit-log
;;   C-c g b   magit-blame
;;   C-c g f   magit-file-dispatch (actions on current file)
;;
;;; Code:

;;;; Magit - Git Porcelain
;;
;; Magit is a complete Git interface. The main entry point is
;; `magit-status' which shows the current repository state.
;;
;; From there, single-key commands perform Git operations:
;;   s = stage, u = unstage, c = commit, P = push, F = pull, etc.
;;
;; Press '?' in any Magit buffer to see available commands.

(use-package magit
  :bind
  (;; Primary entry point
   ("C-c g s" . magit-status)        ; The main Magit buffer
   ("C-x g"   . magit-status)        ; Traditional binding

   ;; Quick access to specific views
   ("C-c g l" . magit-log-current)   ; Log for current branch
   ("C-c g L" . magit-log-all)       ; Log for all branches
   ("C-c g b" . magit-blame)         ; Git blame on current file
   ("C-c g f" . magit-file-dispatch) ; Actions for current file
   ("C-c g d" . magit-diff-dwim))    ; Show diff (smart context)

  :config
  ;; Show fine (word-level) differences in diffs
  ;; Options: nil (none), t (current hunk only), 'all (all hunks)
  (setq magit-diff-refine-hunk 'all)

  ;; Open magit-status in the current window, not a new split
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1)

  ;; Don't ask before saving buffers when running Git commands
  (setq magit-save-repository-buffers 'dontask))

;;;; Forge - GitHub/GitLab Integration
;;
;; Forge adds issue and PR management to Magit.
;; It works with GitHub, GitLab, Gitea, and other forges.
;;
;; First-time setup:
;;   1. Create a GitHub token: Settings → Developer settings → Personal access tokens
;;   2. Add to ~/.authinfo.gpg or ~/.authinfo:
;;      machine api.github.com login YOUR_USERNAME^forge password YOUR_TOKEN
;;
;; Then in a repo:
;;   M-x forge-pull   (or ' f f in magit-status)
;;
;; Forge commands in magit-status:
;;   '       Forge prefix (show forge menu)
;;   ' f f   Fetch issues and PRs (forge-pull)
;;   ' c p   Create pull request
;;   ' c i   Create issue

(use-package forge
  :after magit
  :config
  ;; Forge stores data in a SQLite database
  ;; This requires sqlite3 to be available (installed via Homebrew)

  ;; Number of topics (issues/PRs) to fetch
  (setq forge-topic-list-limit '(100 . 0))  ; 100 open, 0 closed

  ;; Show issues/PRs in status buffer
  (setq forge-add-default-bindings t))

;;;; Git Modes - Syntax Highlighting for Git Files
;;
;; Provides major modes for .gitconfig, .gitignore, etc.

(use-package git-modes
  :mode
  (("\\.gitconfig\\'" . gitconfig-mode)
   ("\\.gitignore\\'" . gitignore-mode)
   ("\\.gitattributes\\'" . gitattributes-mode)
   ("/\\.git/config\\'" . gitconfig-mode)
   ;; Also use gitignore-mode for .dockerignore (same syntax)
   ("\\.dockerignore\\'" . gitignore-mode)))

;;;; Diff-HL - Highlight Uncommitted Changes in Gutter
;;
;; Shows added/modified/removed lines in the fringe (gutter).
;; Useful visual indicator of what you've changed.

(use-package diff-hl
  :hook
  ((prog-mode . diff-hl-mode)
   (org-mode . diff-hl-mode)
   (dired-mode . diff-hl-dired-mode)
   ;; Update highlighting after Magit operations
   (magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  ;; Use the fringe (gutter) for indicators
  (setq diff-hl-draw-borders nil)
  ;; Update diff highlighting on the fly
  (diff-hl-flydiff-mode 1))

;;;; Quick Reference: Common Magit Commands
;;
;; In magit-status buffer:
;;
;; STAGING:
;;   s       Stage file/hunk at point
;;   S       Stage all unstaged changes
;;   u       Unstage file/hunk at point
;;   U       Unstage all staged changes
;;   x       Discard changes at point
;;
;; COMMITTING:
;;   c c     Commit (opens message buffer)
;;   c a     Amend previous commit
;;   c f     Fixup commit (squash into previous)
;;   C-c C-c Confirm commit message
;;   C-c C-k Abort commit
;;
;; BRANCHING:
;;   b b     Switch branch
;;   b c     Create and switch to new branch
;;   b d     Delete branch
;;
;; REMOTE:
;;   P p     Push to remote
;;   P u     Push and set upstream
;;   F p     Pull from remote
;;   f a     Fetch all remotes
;;
;; HISTORY:
;;   l l     Log current branch
;;   l a     Log all branches
;;   d d     Diff (show changes)
;;
;; NAVIGATION:
;;   TAB     Expand/collapse section
;;   n/p     Next/previous section
;;   M-n/p   Next/previous sibling section
;;   ?       Show help for current context

(provide 'nh-git)
;;; nh-git.el ends here
