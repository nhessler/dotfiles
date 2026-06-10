---
name: nh-sync
description: Detailed guide for nh sync — interactive package reconciliation across Homebrew, casks, MAS, ASDF, and Emacs.
trigger: when working with nh sync, Brewfile management, package tracking, skip lists, or reconciling installed vs tracked packages
---

# nh sync — Package Reconciliation

Interactive tool that compares what's installed on the machine vs what's tracked in config files.

## Subcommands

```
nh sync [keep]              # Installed but not tracked → keep/skip prompts (default)
nh sync remove              # Tracked but not installed → remove/keep prompts
nh sync skipped             # Show currently skipped items
nh sync reset               # Clear the skip list
```

## Flags

```
-o, --only <category>       # Run only one category: brew, cask, mas, asdf, emacs
```

Examples:
- `nh sync -o brew` — only sync brew formulae
- `nh sync remove --only cask` — remove mode for casks only
- `nh sync skipped -o mas` — show only MAS skipped items

## Categories (run in this order)

| Category | Installed Source | Tracked In | Interactive? |
|----------|-----------------|------------|-------------|
| Brew Formulae | `brew leaves` | Brewfile `brew "..."` | Yes |
| Brew Casks | `brew list --cask` (excl. nerd fonts) | Brewfile `cask "..."` | Yes |
| Nerd Fonts | `brew search` vs `brew list --cask` | Managed by `bin/install-nerd-fonts.sh` | Informational |
| Mac App Store | `mas list` | Brewfile `mas "...", id: N` | Yes |
| ASDF | `asdf plugin list` | `dot-config/asdf/plugins` | Yes |
| Emacs | `emacs --batch` package-alist vs use-package declarations | Emacs config files | Informational |

## Important Implementation Details

### Brewfile Format
- Sections: brew, cask, mas (taps removed — redundant with qualified names)
- Alphabetically sorted within each section
- MAS entries have column-aligned `id:` — detect alignment from existing entries using `string split "id:"`
- Cask names may be tap-qualified in Brewfile (`d12frosted/emacs-plus/emacs-plus-app`) but `brew list --cask` returns short names — strip tap prefix when comparing
- Use `while read` + `printf '%s\n'` to preserve whitespace when rewriting

### Nerd Font Exclusion
Cask sync excludes nerd fonts matching: `^font-.*(nerd-font|nerd$|-nf$|-nf-)`
This regex matches the same pattern as `bin/install-nerd-fonts.sh`.
Based on: https://gist.github.com/davidteren/898f2dcccd42d9f8680ec69a3a5d350e

### Brew Formulae: keep vs remove
- **Keep mode**: uses `brew leaves` (top-level only)
- **Remove mode**: uses `brew list --formula` (all installed including dependencies)

### MAS Matching
- Uses app ID as the stable identifier (names can change)
- Skip list stores `mas:<id>`
- Display always shows `Name (ID)` with name looked up from `mas list`

### Emacs Drift Detection
- Compares installed packages (from `package-alist`) against `use-package` declarations
- Filters out built-in packages: recentf, saveplace, uniquify, time, eglot, flymake, eldoc, org, css-mode, js, typescript-ts-mode, json-ts-mode, sh-script, conf-mode, ruby-mode, ielm, erlang, heex-ts-mode
- Filters out formal dependencies (from `package-desc-reqs`)
- Informational only — no automatic modifications

### Skip List
- Location: `~/.local/state/nh/sync-skips`
- Format: `type:item` per line (e.g., `brew:exercism`, `mas:497799835`)
- Sorted by section order, then alphabetically within section
- Machine-local, not tracked in git

### Fish Shell Gotchas
- `string match -r` returns both full match AND capture groups — filter with `string match -v -r '^prefix'`
- `string replace -r -- '-otp-.*' ''` — the `--` prevents `-otp` being parsed as a flag
- `seq 0` returns `0` but Fish arrays are 1-indexed — guard with `if test (count $list) -ge 1`
- Use `printf '%s\n'` not `echo` when preserving whitespace in file rewrites
- `test "a" '<' "b"` doesn't work for string comparison — use `printf | sort` instead

### Prompt Behavior
- Requires explicit `k`/`s`/`q` or `r`/`k`/`q` input
- Enter re-prompts (no default action to prevent accidental changes)

## Key Files
- `dot-config/fish/functions/_nh_sync.fish` — all sync logic
- `dot-config/homebrew/Brewfile` — Homebrew/cask/MAS tracking
- `dot-config/asdf/plugins` — ASDF plugin tracking
- `~/.local/state/nh/sync-skips` — skip list (machine-local)
