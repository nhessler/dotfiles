---
name: nh
description: Nathan's personal CLI helper (nh command) for dotfiles management — subcommands for setup, outdated, upgrade, sync, add-keys, and claude.
trigger: when the user mentions nh, dotfiles management, SSH key issues, outdated packages, upgrading packages, or system setup
---

# nh — Personal CLI Helper

`nh` is a Fish shell command with subcommands for managing the dotfiles system.

## Subcommands

### nh setup
Runs the dotfiles setup script (`bin/setup.sh`). Used for initial machine setup or after major config changes.

### nh outdated
Checks for outdated dependencies across: Homebrew, ASDF, Mac App Store, macOS, and Emacs packages. Each category shows two timestamps: last checked and last upgraded. Each `_nh_outdated_*` function calls `_nh_set_last_checked` after running. macOS check actually runs `softwareupdate -l` (slow but accurate).

### nh upgrade
Upgrades packages across the system:
- `brew update && brew upgrade`
- `mas upgrade` (Mac App Store)
- Sets ASDF globals to latest installed versions
- Emacs `package-upgrade-all` in batch mode
- macOS: reminder only (not auto-upgraded due to potential restarts; use `nh outdated mark macos` after applying manually)

Each section calls `_nh_set_last_upgraded` to track upgrade timestamps separately from check timestamps.

### State Files
- `~/.local/state/nh/outdated-checks` — last check timestamp per category
- `~/.local/state/nh/upgrade-runs` — last upgrade timestamp per category

### nh sync
Interactive reconciliation of installed vs tracked packages. **Has its own skill (`nh-sync`) with detailed usage.**

### nh add-keys
Adds SSH keys to the agent using macOS keychain. **Suggest this when the user hits SSH authentication failures with git or GitHub.**

### nh claude
Opens Claude Code in the current directory with a named Ghostty tab (`<folder> - CC`). Uses `claude --continue --name`.

## Key Files
- `dot-config/fish/functions/nh.fish` — main dispatcher
- `dot-config/fish/functions/_nh_*.fish` — subcommand implementations
- `dot-config/fish/completions/nh.fish` — tab completions
- `dot-config/homebrew/Brewfile` — tracked Homebrew packages
- `dot-config/asdf/plugins` — tracked ASDF plugins
- `bin/setup.sh` — machine setup script
- `bin/install-nerd-fonts.sh` — installs all nerd fonts via Homebrew

## Adding New Subcommands
1. Create `dot-config/fish/functions/_nh_<name>.fish`
2. Add `case <name>` to the switch in `nh.fish`
3. Add help text to `_nh_help`
4. Update `dot-config/fish/completions/nh.fish`

## ASDF Version Comparison
When comparing ASDF versions, strip `-otp-*` suffixes before comparing (e.g., `1.19.5-otp-28` matches `1.19.5`). Use `string replace -r -- '-otp-.*' ''` (the `--` is required to prevent Fish from interpreting `-otp` as a flag).

## ASDF Plugin Updates
`asdf plugin update --all` hangs in ASDF 0.18.x. Update plugins individually in a loop instead.
