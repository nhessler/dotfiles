;;; nh-lang-config.el --- Configuration file formats -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Support for various configuration file formats:
;;   - YAML (.yml, .yaml)
;;   - TOML (.toml)
;;   - JSON (.json)
;;   - Dockerfile
;;   - Shell scripts (.sh, .bash, .zsh)
;;   - .env files
;;
;; These are "data" languages that don't typically need LSP support,
;; though some (YAML, JSON) can benefit from schema validation.
;;
;;; Code:

;;;; YAML Mode
;;
;; YAML is everywhere in modern development: CI/CD configs, Kubernetes,
;; Docker Compose, GitHub Actions, etc.
;;
;; Emacs 29+ includes yaml-ts-mode (treesitter), but yaml-mode works well
;; and handles the many edge cases of YAML syntax.
;;
;; For LSP with schema validation (optional):
;;   brew install yaml-language-server

(use-package yaml-mode
  :mode
  (("\\.ya?ml\\'" . yaml-mode)
   ("\\.yml\\.example\\'" . yaml-mode)
   ;; CI/CD configs
   ("\\.github/workflows/.*\\.yml\\'" . yaml-mode)
   ("\\.gitlab-ci\\.yml\\'" . yaml-mode)
   ("\\.circleci/config\\.yml\\'" . yaml-mode)
   ;; Container/K8s configs
   ("docker-compose.*\\.yml\\'" . yaml-mode)
   ("compose.*\\.yml\\'" . yaml-mode))

  :config
  ;; Indentation - YAML standard is 2 spaces
  (setq yaml-indent-offset 2)

  ;; Don't wrap long lines in YAML - it breaks the format
  (add-hook 'yaml-mode-hook
            (lambda ()
              (setq-local fill-column 10000)
              (setq-local truncate-lines t))))

;;;; TOML Mode
;;
;; TOML is popular for Rust (Cargo.toml), Python (pyproject.toml),
;; and various modern config files.
;;
;; There's a toml-ts-mode in Emacs 29+, but toml-mode works well.

(use-package toml-mode
  :mode
  (("\\.toml\\'" . toml-mode)
   ("Cargo\\.toml\\'" . toml-mode)
   ("pyproject\\.toml\\'" . toml-mode)
   ;; Starship prompt config
   ("starship\\.toml\\'" . toml-mode)
   ;; Rust config
   ("\\.cargo/config\\.toml\\'" . toml-mode)))

;;;; JSON Mode
;;
;; JSON is used for package.json, tsconfig.json, .prettierrc, etc.
;;
;; Emacs 29+ includes json-ts-mode, but json-mode provides good
;; formatting and navigation features.
;;
;; For LSP (provides JSON, HTML, CSS servers):
;;   brew install vscode-langservers-extracted

(use-package json-mode
  :mode
  (("\\.json\\'" . json-mode)
   ("\\.jsonc\\'" . json-mode)  ; JSON with comments (VS Code style)
   ("\\.prettierrc\\'" . json-mode)
   ("\\.eslintrc\\'" . json-mode)
   ("\\.babelrc\\'" . json-mode)
   ("tsconfig\\.json\\'" . json-mode)
   ("package\\.json\\'" . json-mode)
   ("package-lock\\.json\\'" . json-mode)
   ("composer\\.json\\'" . json-mode))

  :config
  ;; Indentation - 2 spaces is common
  (setq js-indent-level 2)  ; json-mode uses js-mode indentation

  ;; Format JSON with jq if available
  ;; Install: brew install jq
  (defun nh/json-format-buffer ()
    "Format current JSON buffer using jq."
    (interactive)
    (if (executable-find "jq")
        (shell-command-on-region
         (point-min) (point-max)
         "jq ."
         (current-buffer) t)
      (message "jq not found. Install with: brew install jq"))))

;;;; Dockerfile Mode
;;
;; For editing Dockerfiles. Simple mode with syntax highlighting.

(use-package dockerfile-mode
  :mode
  (("Dockerfile\\'" . dockerfile-mode)
   ("Dockerfile\\.[^/]+\\'" . dockerfile-mode)  ; Dockerfile.dev, etc.
   ("\\.dockerfile\\'" . dockerfile-mode)
   ("Containerfile\\'" . dockerfile-mode)))  ; Podman equivalent

;;;; Shell Script Mode
;;
;; Emacs has excellent built-in shell script support.
;; sh-mode handles bash, sh, zsh, and other shell variants.
;;
;; For LSP (optional - sh-mode is already very capable):
;;   brew install bash-language-server

(use-package sh-script
  :ensure nil  ; Built-in
  :mode
  (("\\.sh\\'" . sh-mode)
   ("\\.bash\\'" . sh-mode)
   ("\\.zsh\\'" . sh-mode)
   ("\\.bashrc\\'" . sh-mode)
   ("\\.bash_profile\\'" . sh-mode)
   ("\\.zshrc\\'" . sh-mode)
   ("\\.profile\\'" . sh-mode)
   ;; Scripts without extension (by shebang - handled by auto-mode-interpreter-regexp)
   )

  :config
  ;; Indentation
  (setq sh-basic-offset 2)
  (setq sh-indentation 2)

  ;; Default to bash for .sh files
  (setq sh-shell-file "/bin/bash"))

;;;; Env File Mode
;;
;; .env files are key=value pairs for environment variables.
;; Used by Docker, Node.js, and many other tools.
;;
;; dotenv-mode provides syntax highlighting for these.

(use-package dotenv-mode
  :mode
  (("\\.env\\'" . dotenv-mode)
   ("\\.env\\.example\\'" . dotenv-mode)
   ("\\.env\\.local\\'" . dotenv-mode)
   ("\\.env\\.development\\'" . dotenv-mode)
   ("\\.env\\.production\\'" . dotenv-mode)
   ("\\.env\\.test\\'" . dotenv-mode)
   ("\\.envrc\\'" . dotenv-mode)))  ; direnv config

;;;; INI/Conf Mode
;;
;; For .ini, .conf, and similar config files.
;; Built-in conf-mode handles these well.

(use-package conf-mode
  :ensure nil  ; Built-in
  :mode
  (("\\.ini\\'" . conf-mode)
   ("\\.conf\\'" . conf-mode)
   ("\\.cfg\\'" . conf-mode)
   ("\\.editorconfig\\'" . conf-mode)
   ("\\.gitconfig\\'" . conf-mode)
   ("\\.npmrc\\'" . conf-mode)
   ("\\.gemrc\\'" . conf-mode)))

;;;; SSH Config Mode
;;
;; For ~/.ssh/config files.

(use-package ssh-config-mode
  :mode
  (("/\\.ssh/config\\'" . ssh-config-mode)
   ("/ssh_config\\'" . ssh-config-mode)
   ("/sshd_config\\'" . ssh-config-mode)
   ("/known_hosts\\'" . ssh-known-hosts-mode)
   ("/authorized_keys\\'" . ssh-authorized-keys-mode)))

;;;; Nginx Mode
;;
;; For nginx configuration files.
;; Useful if you do any local web server configuration.

(use-package nginx-mode
  :mode
  (("/nginx/.*\\.conf\\'" . nginx-mode)
   ("\\.nginx\\'" . nginx-mode)
   ("nginx\\.conf\\'" . nginx-mode)))

;;;; Apache Mode
;;
;; For Apache httpd configuration.

(use-package apache-mode
  :mode
  (("\\.htaccess\\'" . apache-mode)
   ("httpd\\.conf\\'" . apache-mode)
   ("apache2?\\.conf\\'" . apache-mode)
   ("sites-\\(available\\|enabled\\)/.*\\'" . apache-mode)))

;;;; Mermaid Mode
;;
;; For Mermaid diagrams (flowcharts, sequence diagrams, etc.)
;; Often used in Markdown documentation.

(use-package mermaid-mode
  :mode "\\.mmd\\'"
  :config
  ;; If you have mermaid-cli installed, you can render diagrams
  ;; Install: brew install mermaid-cli
  (setq mermaid-mmdc-location "mmdc"))

;;;; Caddyfile Mode
;;
;; For Caddy web server configuration (from your caddy/ directory).
;; Note: There isn't a well-maintained caddyfile-mode package,
;; but we can use a simple conf-mode derivative.

;; Simple Caddyfile support using conf-mode
(add-to-list 'auto-mode-alist '("Caddyfile\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.caddyfile\\'" . conf-mode))

;;;; Just Mode
;;
;; For Justfiles (command runner, like Make but simpler).
;; Install: brew install just

(use-package just-mode
  :mode
  (("justfile\\'" . just-mode)
   ("Justfile\\'" . just-mode)
   ("\\.just\\'" . just-mode)))

;;;; Keybinding Notes
;;
;; Most config file modes don't need special keybindings since
;; they're primarily for viewing/editing data.
;;
;; Useful general commands:
;;   C-c C-c   In many modes, this comments/uncomments
;;   C-M-\     Indent region
;;   M-;       Toggle line comment
;;
;; JSON-specific:
;;   nh/json-format-buffer  Format JSON using jq

(with-eval-after-load 'json-mode
  (define-key json-mode-map (kbd "C-c m f") #'nh/json-format-buffer))

(provide 'nh-lang-config)
;;; nh-lang-config.el ends here
