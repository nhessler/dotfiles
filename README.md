# Dotfiles

Personal macOS dotfiles.

## Quick Start

**Prerequisites:**
- macOS (Apple Silicon or Intel)
- SSH key for GitHub ([setup guide](docs/ssh-setup.md))

**Phase 1: Bootstrap** (one-time setup)
```shell
curl -fsSL https://raw.githubusercontent.com/nhessler/dotfiles/main/bin/bootstrap.sh | bash
```

**Phase 2: Setup** (packages & languages)
```shell
~/Projects/nhessler/dotfiles/bin/setup.sh
```

Use `--skip-mas` if not signed into the App Store.

**Phase 3: Post-Setup** (manual steps)

See [docs/post-setup.md](docs/post-setup.md) for app logins, license keys, and sync setup.

## What Gets Installed

### Bootstrap (Phase 1)
- Xcode Command Line Tools
- Rosetta 2 (Apple Silicon only)
- Security hardening: FileVault check, firewall, TouchID sudo
- Homebrew
- ASDF version manager
- Dotfile symlinks
- Software updates

### Setup (Phase 2)
- Brewfile packages (CLI tools, casks, App Store apps)
- ASDF languages: Ruby, Erlang, Elixir, Node.js
- Fish shell (set as default)
- Nerd fonts
- macOS system defaults

## Directory Structure

```
dotfiles/
├── bin/                    # Setup scripts
│   ├── bootstrap.sh        # Phase 1: one-time setup
│   ├── setup.sh            # Phase 2: packages & languages
│   ├── install-nerd-fonts.sh
│   └── macos-defaults.sh
├── dot-config/             # → ~/.config/
│   ├── fish/               # Shell
│   ├── git/                # Git config & hooks
│   ├── homebrew/           # Brewfile
│   ├── emacs/              # Editor
│   ├── starship/           # Prompt
│   └── ghostty/            # Terminal
├── dot-hammerspoon/        # → ~/.hammerspoon/
├── dot-claude/             # → ~/.claude/
├── caddy/                  # Local dev server (gitignored)
└── docs/                   # Additional guides
```

## Helpers

### Postgres via Docker

Run multiple Postgres versions using Docker with persistent volumes. This makes it easy to switch versions or run multiple instances on different ports.

**First-time setup** (create the volume directory):
```shell
mkdir -p ~/docker/volumes/postgres-17
```

**Run Postgres:**
```shell
docker run --rm --name postgres-17 -p 127.0.0.1:5432:5432 \
  -v $HOME/docker/volumes/postgres-17:/var/lib/postgresql/data \
  -e POSTGRES_PASSWORD=postgres -d postgres:17-alpine
```

To run multiple versions, use different ports and volume directories:
```shell
# Postgres 15 on port 5433
docker run --rm --name postgres-15 -p 127.0.0.1:5433:5432 \
  -v $HOME/docker/volumes/postgres-15:/var/lib/postgresql/data \
  -e POSTGRES_PASSWORD=postgres -d postgres:15-alpine
```

### Git Commit Signing

Set up SSH commit signing for a repository:
```shell
git secure -n "Your Name" -e you@example.com -k ~/.ssh/id_ed25519
```

### Nerd Fonts

Install all Nerd Fonts (run automatically by setup.sh, or manually):
```shell
~/Projects/nhessler/dotfiles/bin/install-nerd-fonts.sh
```

## Inspirations

- [Strap](https://github.com/MikeMcQuaid/strap) - Mike McQuaid's macOS setup
- [GitHub Does Dotfiles](https://dotfiles.github.io/)

## Password Security

Resources for generating strong passphrases:

- [Diceware](https://theworld.com/~reinhold/diceware.html)
- [EFF Passphrase Guide](https://www.eff.org/deeplinks/2016/07/new-wordlists-random-passphrases)
- Word lists:
  [EFF Large](https://www.eff.org/files/2016/07/18/eff_large_wordlist.txt) ·
  [EFF Short](https://www.eff.org/files/2016/09/08/eff_short_wordlist_1.txt) ·
  [Beale](https://theworld.com/~reinhold/beale.wordlist.asc) ·
  [Diceware](https://theworld.com/~reinhold/diceware.wordlist.asc)
