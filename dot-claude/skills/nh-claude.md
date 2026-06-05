---
name: nh-claude
description: Detailed guide for nh claude — multi-account Claude Code with cwd-based auto-routing, isolated config dirs, and convention↔multi-account migration.
trigger: when working with nh claude, multiple Claude Code accounts, CLAUDE_CONFIG_DIR, account isolation, ~/.claude.d/ layout, the org→account routing registry, or the convention-mode (~/.claude) vs multi-account-mode setup
---

# nh claude — Multi-Account Claude Code

Launches Claude Code with cwd-based account auto-routing. Each account has its
own isolated `~/.claude.d/<account>/` config (auth, conversation history, MCP
servers, etc). Designed to keep client work cleanly separated from personal
work under strict-isolation compliance.

## Launch Subcommands

```
nh claude            # continue most recent session (default)
nh claude new        # fresh session
nh claude continue   # explicit continue
nh claude help       # help text
```

Every launch:
- Resolves an account from cwd (see Routing below).
- Sets `CLAUDE_CONFIG_DIR=~/.claude.d/<account>`.
- Appends ` [<account>]` to the Ghostty tab name.
- Prints a stderr line: `Using Claude account: <name> (<config-dir>)`.

## Configuration Subcommands

```
nh claude account                  Show the active account for cwd
nh claude account add <name>       Symlink ~/.claude.d/<name> → dot-claude.d/<name>/
nh claude account remove <name>    Unlink ~/.claude.d/<name> (does not delete the dir)
nh claude account list             Show all accounts and the orgs that route to each

nh claude org map <path> <account>   Route ~/Projects/<path>/ to <account>
nh claude org unmap <path>           Remove a mapping
nh claude org list                   Show all mappings + the catch-all fallback
```

`rm`/`ls` aliases work where you'd expect.

## Routing

The active account for the current shell is determined by `_nh_account_for_cwd`:

1. Cwd must be under `~/Projects/`. If not → fall back to `nh`.
2. Strip `~/Projects/` prefix, then for each line in
   `~/.local/state/nh/account-orgs`, check if cwd-relative-path equals the LHS
   or starts with `LHS/`.
3. Among matches, **the longest LHS wins** (most specific). So a
   `lolollc/qrlite=experiment` entry overrides a `lolollc=ll` entry when cwd
   is inside `lolollc/qrlite/`.
4. No match → fall back to `nh`.

Registry LHS forms:
- `<org>` (e.g., `lolollc`) — matches every repo under `~/Projects/<org>/`.
- `<org>/<repo>` (e.g., `lolollc/qrlite`) — matches that specific repo's subtree.
- Absolute paths are **not** supported yet.

## Two Modes of Operation

Convention mode (one account, default):
```
~/.claude → ~/Projects/nhessler/dotfiles/dot-claude/   # everything lives here
```

Multi-account mode (two or more accounts):
```
~/.claude               (gone — does NOT exist)
~/.claude.d/
├── nh → dotfiles/dot-claude/                # default account, legacy location
├── ll → dotfiles/dot-claude.d/ll/
└── <other> → dotfiles/dot-claude.d/<other>/
```

Transitions happen automatically:
- `account add <name>` while in convention mode → **migrate** (rm `~/.claude`,
  create `~/.claude.d/`, symlink `nh` and the new account).
- `account remove <name>` while in multi-account mode that would leave only
  `nh` → **demigrate** (unlink `nh`, rmdir `~/.claude.d`, recreate `~/.claude`).

## Conventions and Rules

- **Default account is `nh`.** It's auto-managed — you can't add or remove it
  via `account add`/`remove`. It always maps to `dot-claude/` in the dotfiles
  repo (the legacy convention-mode source).
- **Non-default accounts live at `dot-claude.d/<name>/` in the dotfiles repo.**
  `nh claude account add <name>` errors if `dot-claude.d/<name>/` doesn't
  exist — you must create it first.
- **Symlinks always point at dotfiles.** No per-account dir lives outside the
  repo. This keeps every account's settings/skills/hooks tracked (the runtime
  state — `.claude.json`, `projects/`, `sessions/`, etc. — is gitignored via
  globbed patterns).
- **Remove is unlink, not delete.** `nh claude account remove ll` removes the
  symlink and drops org mappings to that account, but `dot-claude.d/ll/`
  stays put so re-adding later is friction-free.
- **Org mapping order matters at resolution time, not file order.** Longest
  LHS wins regardless of where it appears in `account-orgs`.
- **Override per-account path via `~/.local/state/nh/accounts`** (lines like
  `<name>=<absolute-path>`). Rare — useful if an account's config needs to
  live somewhere unusual, e.g., on encrypted external storage.

## State Files

```
~/.claude.d/<account>/              Each account's symlinked config root
~/.local/state/nh/account-orgs      <path>=<account> mappings (one per line)
~/.local/state/nh/accounts          (Optional) <name>=<absolute-path> overrides
```

## Common Tasks

### Add a client account

1. Create the source dir in dotfiles: `mkdir -p dot-claude.d/<name>/`
2. Register it: `nh claude account add <name>`
3. Route an org to it: `nh claude org map <org> <name>`
4. `cd ~/Projects/<org>/<repo> && nh claude new` — first launch hits `/login`.

### Reassign an org to a different account

```
nh claude org unmap <org>
nh claude org map <org> <new-account>
```

`map` refuses to overwrite an existing mapping by design — explicit unmap
prevents accidents.

### See what's wired up

```
nh claude account list   # accounts with orgs grouped under each
nh claude org list       # paths sorted, with fallback footer
nh claude account        # which account would launch from this cwd
```

### Remove a client account, keep the dotfiles

```
nh claude account remove <name>
```

Drops the symlink and all org mappings pointing at it. The
`dot-claude.d/<name>/` dir in dotfiles is untouched. If `<name>` was the last
non-default account, the system demigrates back to convention mode.

## Gotchas and Failure Modes

- **`~/.claude` and `~/.claude.d/` should never coexist.** If they do (e.g.,
  half-finished manual edit), `account add` will refuse to migrate. Inspect
  with `ls -la ~/.claude*` and reconcile manually.
- **`account add <name>` fails if `dot-claude.d/<name>/` doesn't exist.**
  Deliberate guardrail — forces you to set up the destination first rather
  than silently creating an empty dir that would auto-populate with random
  account state on first launch.
- **CLAUDE_CONFIG_DIR doesn't redirect everything.** `~/.claude.d/<account>/`
  isolates auth (`.claude.json`), conversation history (`projects/`,
  `sessions/`), backups, plugins, and account-specific settings/skills/hooks/
  agents *if you scope them to that account's dir*. It does NOT redirect:
  - `~/Library/Caches/claude-cli-nodejs/<encoded-cwd>/` (per-project, not
    per-account; minimal leak risk)
  - `~/Library/Logs/Claude/` (mostly desktop-app logs; minimal CLI usage)
  - Anything under `$HOME` that Claude Code itself doesn't read via
    `CLAUDE_CONFIG_DIR` (verified empirically — see git history for tests)
- **Adding an org that overlaps an existing one is allowed.** With
  `lolollc=ll` and `lolollc/qrlite=experiment` both present, longest-match
  resolves correctly. But mapping `lolollc/qrlite` to one account and
  `lolollc/qrlite/sub` to another is also legal — be intentional, not
  accidental.

## Key Files

- `dot-config/fish/functions/_nh_claude.fish` — dispatcher; sets
  `CLAUDE_CONFIG_DIR` and launches `claude`.
- `dot-config/fish/functions/_nh_claude_account.fish` — `account add/remove/
  list` plus the migration/demigration helpers.
- `dot-config/fish/functions/_nh_claude_org.fish` — `org map/unmap/list`.
- `dot-config/fish/functions/_nh_account_for_cwd.fish` — longest-match
  resolver against `~/.local/state/nh/account-orgs`.
- `dot-config/fish/functions/_nh_account_config_dir.fish` — resolves
  `<account>` to a config-dir path (default `~/.claude.d/<name>`, with
  optional override registry).
- `dot-config/fish/completions/nh.fish` — completions for the full
  account/org sub-surface.
- `dot-claude/` — source of the `nh` (default) account's tracked config.
- `dot-claude.d/<name>/` — source of each non-default account's tracked
  config.

## Related Skills

- `nh` — overview of the whole `nh` CLI.
- `nh-caddy`, `nh-sync` — sibling subcommand guides.
