;;; nh-lang-erlang.el --- Erlang language configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Erlang development setup with:
;;   - erlang-mode (from OTP or MELPA)
;;   - Eglot integration for LSP (erlang_ls)
;;
;; Erlang is the foundation for Elixir and powers:
;;   - Distributed systems (RabbitMQ, ejabberd)
;;   - Telecom infrastructure
;;   - Real-time systems
;;
;; Understanding Erlang helps when diving into Elixir internals,
;; reading OTP source code, or debugging BEAM issues.
;;
;;;; LSP Setup
;;
;; Install erlang_ls via Homebrew:
;;   brew install erlang_ls
;;
;;; Code:

;;;; Erlang Mode
;;
;; Major mode for editing Erlang source files.
;;
;; erlang-mode can come from two places:
;;   1. Bundled with OTP (if Erlang installed via asdf/Homebrew)
;;   2. MELPA package
;;
;; We try OTP's version first, fall back to MELPA.

;; First, try to find erlang-mode from OTP installation
(let ((erlang-emacs-dir
       (when (executable-find "erl")
         ;; Get Erlang lib directory and find emacs tools
         (let* ((erl-root (string-trim
                           (shell-command-to-string
                            "erl -noshell -eval 'io:format(\"~s\", [code:root_dir()])' -s init stop 2>/dev/null")))
                (emacs-dir (expand-file-name "lib/tools-*/emacs" erl-root)))
           (car (file-expand-wildcards emacs-dir))))))
  (when (and erlang-emacs-dir (file-directory-p erlang-emacs-dir))
    (add-to-list 'load-path erlang-emacs-dir)))

;; Now configure erlang-mode (from OTP or MELPA)
(use-package erlang
  :mode
  (("\\.erl\\'" . erlang-mode)
   ("\\.hrl\\'" . erlang-mode)      ; Header files
   ("\\.xrl\\'" . erlang-mode)      ; Leex files
   ("\\.yrl\\'" . erlang-mode)      ; Yecc files
   ("\\.app\\'" . erlang-mode)      ; Application resource files
   ("\\.app\\.src\\'" . erlang-mode)
   ("rebar\\.config\\'" . erlang-mode)
   ("relx\\.config\\'" . erlang-mode)
   ("sys\\.config\\'" . erlang-mode))

  :hook
  ;; Start Eglot automatically for Erlang files
  (erlang-mode . eglot-ensure)

  :config
  ;; Indentation - Erlang convention is 4 spaces, but 2 is also common
  (setq erlang-indent-level 2)

  ;; Don't auto-insert skeleton templates (they can be annoying)
  (setq erlang-electric-commands nil))

;;;; Eglot: Erlang Language Server Configuration
;;
;; erlang_ls provides navigation, completion, and diagnostics.
;; Install via: brew install erlang_ls

(with-eval-after-load 'eglot
  ;; erlang_ls should be found in PATH via Homebrew
  (add-to-list 'eglot-server-programs
               '(erlang-mode . ("erlang_ls"))))

;;;; Erlang Commands
;;
;; Helpers for running Erlang commands.
;; These use rebar3 which is the standard Erlang build tool.

(defun nh/erlang-compile ()
  "Compile Erlang project with rebar3."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "rebar3 compile")))

(defun nh/erlang-test ()
  "Run Erlang tests with rebar3."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "rebar3 eunit")))

(defun nh/erlang-dialyzer ()
  "Run Dialyzer type checker."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "rebar3 dialyzer")))

(defun nh/erlang-shell ()
  "Start Erlang shell for the project."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "rebar3 shell")))

;;;; Keybindings
;;
;; Erlang-specific bindings.

(with-eval-after-load 'erlang
  (define-key erlang-mode-map (kbd "C-c m c") #'nh/erlang-compile)
  (define-key erlang-mode-map (kbd "C-c m t") #'nh/erlang-test)
  (define-key erlang-mode-map (kbd "C-c m d") #'nh/erlang-dialyzer)
  (define-key erlang-mode-map (kbd "C-c m s") #'nh/erlang-shell))

;;;; Notes on Erlang + Elixir
;;
;; When working with Elixir, you'll occasionally encounter Erlang:
;;   - Calling Erlang stdlib: :erlang, :ets, :gen_server, etc.
;;   - Reading OTP source to understand behavior
;;   - Debugging low-level BEAM issues
;;
;; Key differences from Elixir:
;;   - Variables start with uppercase: Variable, not variable
;;   - Atoms are lowercase or 'quoted': atom, 'Atom With Spaces'
;;   - Modules are atoms: -module(my_module).
;;   - Pattern matching uses = same as Elixir
;;   - Records (like Elixir structs but older): #record{field=value}

(provide 'nh-lang-erlang)
;;; nh-lang-erlang.el ends here
