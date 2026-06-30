---
name: nh-claude
description: Detailed guide for nh claude — multi-account Claude Code with cwd-based auto-routing, isolated config dirs, and convention↔multi-account migration.
trigger: when working with nh claude, multiple Claude Code accounts, CLAUDE_CONFIG_DIR, account isolation, ~/.claude.d/ layout, the path→account routing registry, or the convention-mode (~/.claude) vs multi-account-mode setup
---

# nh claude — Multi-Account Claude Code

Launches Claude Code with cwd-based account auto-routing. Each account has its
own isolated `~/.claude.d/<account>/` config (auth, conversation history, MCP
servers, etc). Designed to keep client work cleanly separated from personal
work under strict-isolation compliance.

## Command Surface

```
nh claude                          # continue most recent session (default)
nh claude new                      # fresh session
nh claude continue                 # explicit continue
nh claude current                  # show active account for cwd
nh claude register <account>               # register an account (~/.claude.d/<account> → dot-claude.d/<account>/)
nh claude unregister <account>            # unregister (does not delete dot-claude.d/<account>/)
nh claude map <path> <account>     # route ~/Projects/<path>/ to <account>
nh claude unmap <path>             # remove a mapping
nh claude list                     # unified view: accounts with the paths that route to each
nh claude help
```

`rm`/`ls` aliases work where you'd expect.

## Launch Behavior

Every `nh claude` / `nh claude new` / `nh claude continue`:
- Resolves an account from cwd (see Routing below).
- Sets `CLAUDE_CONFIG_DIR=~/.claude.d/<account>`.
- Appends ` [<account>]` to the Ghostty tab name.
- Prints a stderr line: `Using Claude account: <account> (<config-dir>)`.

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
- Absolute paths are **not** supported.

## Bare `claude` Wrapper

`dot-config/fish/functions/claude.fish` wraps the `claude` command itself so
that *every* invocation — not just `nh claude` — routes to the right account.
Before exec'ing the real binary it sets `CLAUDE_CONFIG_DIR` via the same
`_nh_account_for_cwd` resolver, so ad-hoc commands like `claude mcp add` or
`claude config` land on whichever account owns the current directory.

- **Only injected when `CLAUDE_CONFIG_DIR` is unset.** An explicit
  `set -x CLAUDE_CONFIG_DIR …` / `env … claude …`, or a running session's
  already-exported var, wins and is not overridden.
- **Falls back to `nh`** when cwd maps to nothing.
- Uses `command claude` internally to reach the real binary (no recursion).
- **`nh claude` is unaffected** — it execs the binary through `env`, which
  bypasses the fish function entirely.

Why it exists: a raw `claude` in a plain shell has no `CLAUDE_CONFIG_DIR` set
(the `nh claude` launcher only sets it for its own subprocess), so without the
wrapper it would fall back to a stray `~/.claude` — neither account. The
wrapper makes ad-hoc commands route correctly and stops that orphan config
from being created/written. (Added 2026-06-30.)

To force a specific account regardless of cwd, set it explicitly:
`env CLAUDE_CONFIG_DIR=$HOME/.claude.d/ll claude mcp add …`

### Why two commands (`claude` wrapper *and* `nh claude`)?

They're layers, not duplicates — deliberately kept separate (decided
2026-06-30):

- **`claude` (wrapper) = correctness layer.** Transparent: behaves exactly
  like real claude, just pinned to the right account. Use for `claude mcp
  add`, `claude config`, `claude -p …`, tooling, and habit. Transparency is
  the whole point, so it must NOT grow opinions (no `--continue`, no tab
  rename) — those would surprise scripts/tools and there's no reliable way to
  tell a session launch from `claude mcp …`/`claude -p …`.
- **`nh claude` = ergonomics + admin layer.** Opinionated session launcher
  (tab naming, `--continue` by default, account banner) plus the account
  subcommands (`register`/`map`/`list`/…). Use it to *start work* and *manage
  accounts*.

Not consolidated because each full merge is worse: folding everything into
`claude` breaks its transparency; folding everything into `nh claude`
reintroduces the stray-`~/.claude` footgun (any raw `claude` would misroute)
and forces an `nh ` prefix on every command. Crucially there's **no routing
logic to consolidate** — both delegate to `_nh_account_for_cwd` +
`_nh_account_config_dir`, so they cannot disagree about which account a
directory belongs to. `nh claude` also stays self-contained (sets the dir via
its own `env`) so it works even if the wrapper is removed.

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
- `nh claude register <account>` while in convention mode → **migrate** (rm
  `~/.claude`, create `~/.claude.d/`, symlink `nh` and the new account).
- `nh claude unregister <account>` while in multi-account mode that would leave only
  `nh` → **demigrate** (unlink `nh`, rmdir `~/.claude.d`, recreate
  `~/.claude`).

## `nh claude list` Output

Single account (convention mode):
```
nh  →  ~/.claude  →  dot-claude  (default)
    (everything else)
```

Multiple accounts:
```
ll  →  ~/.claude.d/ll  →  dot-claude.d/ll
    lolollc
nh  →  ~/.claude.d/nh  →  dot-claude  (default)
    (everything else)
```

When a sub-path is mapped to a different account (a carve-out), the parent
mapping shows an inline `(except ...)` annotation:
```
experiment  →  ~/.claude.d/experiment  →  dot-claude.d/experiment
    lolollc/qrlite
ll          →  ~/.claude.d/ll  →  dot-claude.d/ll
    lolollc (except lolollc/qrlite)
nh          →  ~/.claude.d/nh  →  dot-claude  (default)
    (everything else)
```

Exceptions are only direct ancestor/descendant relationships and only annotate
when the accounts differ. If a parent and a sub-path both map to the same
account, the sub-path just appears as a separate row underneath the same
account heading (no exception annotation needed).

## Conventions and Rules

- **Default account is `nh`.** It's auto-managed — you can't register or
  unregister it. It always maps to `dot-claude/` in the dotfiles repo
  (the legacy convention-mode source).
- **Non-default accounts live at `dot-claude.d/<account>/` in the dotfiles
  repo.** `nh claude register <account>` errors if `dot-claude.d/<account>/` doesn't
  exist — you must create it first.
- **Symlinks always point at dotfiles.** No per-account dir lives outside the
  repo. This keeps every account's settings/skills/hooks tracked (the runtime
  state — `.claude.json`, `projects/`, `sessions/`, etc. — is gitignored via
  globbed patterns).
- **Unregister is unlink, not delete.** `nh claude unregister ll` removes the
  symlink and drops org mappings to that account, but `dot-claude.d/ll/`
  stays put so re-registering later is friction-free.
- **Mapping order in the registry file doesn't matter.** Longest LHS wins at
  resolution time regardless of file order.
- **Override per-account path via `~/.local/state/nh/accounts`** (lines like
  `<account>=<absolute-path>`). Rare — useful if an account's config needs to
  live somewhere unusual, e.g., on encrypted external storage.

## State Files

```
~/.claude.d/<account>/              Each account's symlinked config root
~/.local/state/nh/account-orgs      <path>=<account> mappings (one per line)
~/.local/state/nh/accounts          (Optional) <account>=<absolute-path> overrides
```

## Common Tasks

### Add a client account

1. Create the source dir in dotfiles: `mkdir -p dot-claude.d/<account>/`
2. Register it: `nh claude register <account>`
3. Route an org to it: `nh claude map <org> <account>`
4. `cd ~/Projects/<org>/<repo> && nh claude new` — first launch hits `/login`.

### Reassign a path to a different account

```
nh claude unmap <path>
nh claude map <path> <new-account>
```

`map` refuses to overwrite an existing mapping by design — explicit unmap
prevents accidents.

### See what's wired up

```
nh claude list      # unified view: accounts with their mapped paths
nh claude current   # which account would launch from this cwd
```

### Remove a client account, keep the dotfiles

```
nh claude unregister <account>
```

Drops the symlink and all org mappings pointing at it. The
`dot-claude.d/<account>/` dir in dotfiles is untouched. If `<account>` was the last
non-default account, the system demigrates back to convention mode.

## Gotchas and Failure Modes

- **`~/.claude` and `~/.claude.d/` should never coexist.** If they do (e.g.,
  half-finished manual edit), `register` will refuse to migrate. Inspect with
  `ls -la ~/.claude*` and reconcile manually. (The `claude` fish wrapper, added
  2026-06-30, prevents a bare `claude` from *creating* this stray dir — see
  "Bare `claude` Wrapper" — but won't remove one that already exists.)
- **`add <account>` fails if `dot-claude.d/<account>/` doesn't exist.** Deliberate
  guardrail — forces you to set up the destination first rather than silently
  creating an empty dir that would auto-populate with random account state on
  first launch.
- **CLAUDE_CONFIG_DIR doesn't redirect everything.** `~/.claude.d/<account>/`
  isolates auth (`.claude.json`), conversation history (`projects/`,
  `sessions/`), backups, plugins, and account-specific settings/skills/hooks/
  agents *if you scope them to that account's dir*. It does NOT redirect:
  - `~/Library/Caches/claude-cli-nodejs/<encoded-cwd>/` (per-project, not
    per-account; minimal leak risk)
  - `~/Library/Logs/Claude/` (mostly desktop-app logs; minimal CLI usage)
  - Anything under `$HOME` that Claude Code itself doesn't read via
    `CLAUDE_CONFIG_DIR` (verified empirically — see git history for tests).
- **Adding overlapping mappings is allowed.** Longest-match resolves
  correctly. List view annotates the carve-out as `(except ...)` when
  accounts differ.

## Key Files

- `dot-config/fish/functions/_nh_claude.fish` — dispatcher and all sub-command
  handlers (current, register, unregister, map, unmap, list) plus the
  migration/demigration helpers and list-with-exceptions logic.
- `dot-config/fish/functions/_nh_account_for_cwd.fish` — longest-match
  resolver against `~/.local/state/nh/account-orgs`.
- `dot-config/fish/functions/_nh_account_config_dir.fish` — resolves
  `<account>` to a config-dir path (default `~/.claude.d/<account>`, with
  optional override registry).
- `dot-config/fish/completions/nh.fish` — completions for the full claude
  surface.
- `dot-config/fish/functions/claude.fish` — wraps the bare `claude` command to
  auto-set `CLAUDE_CONFIG_DIR` by cwd (see "Bare `claude` Wrapper" above).
- `dot-claude/` — source of the `nh` (default) account's tracked config.
- `dot-claude.d/<account>/` — source of each non-default account's tracked
  config.

## Related Skills

- `nh` — overview of the whole `nh` CLI.
- `nh-caddy`, `nh-sync` — sibling subcommand guides.
