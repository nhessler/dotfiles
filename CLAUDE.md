# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

Personal dotfiles repository for macOS development environment. Files use a `dot-` prefix convention (e.g., `dot-config`, `dot-hammerspoon`) to organize dotfiles that will be symlinked to their `~/.` equivalents.

## Key Commands

### Initial Machine Setup (Two Phases)

**Phase 1: Bootstrap** (one-time, requires SSH key for GitHub)
```shell
curl -fsSL https://raw.githubusercontent.com/nhessler/dotfiles/main/bin/bootstrap.sh | bash
```
Sets up: Xcode CLI tools, clones dotfiles via SSH, security hardening (FileVault, firewall, TouchID sudo), Homebrew, ASDF (tool only), dotfile symlinks, software updates.

**Phase 2: Install** (run after bootstrap completes)
```shell
~/Projects/nhessler/dotfiles/bin/install.sh
```
Installs: Brewfile packages, ASDF plugins and languages (ruby, erlang, elixir, nodejs), sets Fish as default shell.

**Optional: macOS Defaults**
```shell
~/Projects/nhessler/dotfiles/bin/macos-defaults.sh
```
Applies preferred macOS system settings (edit before running).

### Install Homebrew Packages
```shell
brew bundle --file=dot-config/homebrew/Brewfile
```

### Git Commit Signing Setup
Configure a local repo to sign commits with SSH keys:
```shell
git secure -n "Name" -e email@example.com -k ~/.ssh/id_ed25519
```
This is a custom Fish function that sets up local git config for SSH commit signing and manages `~/.config/git/allowed_signers`.

### Helper: Install All Nerd Fonts
```shell
brew search '/font-.*-nerd-font/' | awk '{ print $1 }' | xargs -I{} brew install --cask {} || true
```

### Run Postgres via Docker
```shell
docker run --rm --name postgres-15 -p 127.0.0.1:5432:5432 -v $HOME/docker/volumes/postgres-15:/var/lib/postgresql/data -e POSTGRES_PASSWORD=postgres -d postgres:15-alpine
```

## Working in This Repo

**Always edit `dot-*` files**, not the symlinked files in `~/`. Edits take effect immediately since they're symlinked.

**Fish function caching**: After editing fish functions, reload with:
```shell
source ~/.config/fish/functions/<name>.fish
# or: functions -e <name>
```

**Prefer `dot-config/`** for new configurations when possible (XDG base directory spec). There's an ongoing effort to migrate configs into `dot-config/`.

**`dot-claude/` is off-limits** - Claude Code's own config lives here. The user will edit these files manually.

**Never push to GitHub** - Only commit locally. The user will decide when to push.

## Architecture

### Directory Structure
- `bin/` - Setup scripts (bootstrap.sh, install.sh, macos-defaults.sh)
- `dot-config/` → `~/.config/` - XDG config (fish, git, starship, ghostty, bat, gh, homebrew, emacs)
- `dot-hammerspoon/` → `~/.hammerspoon/` - macOS window management automation
- `dot-claude/` → Claude Code CLI settings (access restricted in settings.json)
- `caddy/` → Local development web server config (gitignored, per-machine)
- `docs/` → Documentation (ssh-setup.md, local-dev-setup.md)

### Shell Environment (Fish)
The shell initializes in this order (`dot-config/fish/config.fish`):
1. Homebrew PATH (`/opt/homebrew/bin`)
2. Color syntax (Monokai theme)
3. direnv, zoxide, asdf, starship, fzf integrations
4. GPG_TTY for commit signing

Custom functions live in `dot-config/fish/functions/` (e.g., `git-secure.fish`).

### Development Tools
- **Version Manager**: ASDF (Ruby, Elixir, Go, Node.js, Rust)
- **Editor**: Emacs with Projectile (projects in `~/Projects/`), Magit, Forge, LSP
- **Terminal**: Ghostty with Monokai Classic theme
- **Prompt**: Starship (minimal config)

### Git Configuration
- Editor: Emacs with Ediff for merge conflicts
- Default branch: main
- Pull strategy: fast-forward only
- Custom aliases: `git logg` (graph log), `git secure` (SSH signing setup)

### Window Management
Hammerspoon provides window tiling with `Ctrl+Alt+Cmd+W` hotkey (halves, quarters, thirds, sixths).
