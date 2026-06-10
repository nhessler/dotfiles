---
name: nh-caddy
description: Detailed guide for nh caddy — per-project Caddyfile.local management for local Caddy reverse-proxying.
trigger: when working with nh caddy, local Caddy server setup, .test TLD development, reverse_proxy configuration, per-project Caddyfile.local files, or the master Caddyfile at ~/Projects/nhessler/dotfiles/caddy/Caddyfile
---

# nh caddy — Per-Project Caddyfile.local Management

Manages local Caddy reverse-proxy configuration as a master Caddyfile that
`import`s per-project `Caddyfile.local` files. Each project owns its own site
blocks; the master file is just a thin index of imports.

## Subcommands

```
nh caddy new [project] [--domain D] [--port P] [--static]
nh caddy remove [project] [--purge]
nh caddy list [--paths]
nh caddy reload
```

`rm` is an alias for `remove`. `ls` is an alias for `list`.

## Project Resolution

If `[project]` is omitted, defaults to the current working directory (the
basename becomes the project name).

If a bare name is given, resolution order:
1. Registry file `~/.local/state/nh/projects` (name=path lines)
2. `~/Projects/$name` (direct child, e.g. an org dir)
3. `~/Projects/*/$name` (one level deep — matches `~/Projects/<org>/<name>`)

If ambiguous (multiple `~/Projects/*/$name` matches), errors with a list of
candidates and suggests the explicit `org/name` syntax.

If `<org>/<name>` is given, resolves directly to `~/Projects/<org>/<name>`
with no ambiguity check.

`nh caddy new` auto-registers the resolved (name, path) into the registry,
so later `nh caddy remove <name>` works from any cwd.

## Template Modes for `new`

Three template variants, chosen by flags:

| Flags | Template |
|-------|----------|
| `--port N` | Active `reverse_proxy localhost:N` |
| `--static` | Active `file_server` rooted at the project dir |
| Neither | Commented examples (both patterns shown, user fills in) |

Domain defaults to `<project>.test`. Override with `--domain <host>`.

All templates include `tls internal` for Caddy's local CA.

## Master Caddyfile

Defaults to `~/Projects/nhessler/dotfiles/caddy/Caddyfile`. Override with
the `NH_CADDY_FILE` environment variable (useful for testing).

The directory `caddy/` is gitignored as per-machine config. The user has
set up Caddy (via Homebrew) to load this file — most likely via a symlink
or service config pointing at it. Don't assume how Caddy itself is started;
just trust the master file path.

## `nh caddy list` Output

Greps the master Caddyfile for `^import .../Caddyfile.local$` lines, then
for each opens the file and extracts top-level site-block openers (lines
that start with a non-comment non-whitespace char and end with `{`). Splits
comma-separated domains.

Output format:
```
<project-name>: <domain1>, <domain2>, ...
```

With `--paths`:
```
<project-name>: <domains>  →  /full/path/Caddyfile.local
```

Missing imported files (broken imports) are shown as `(missing)`.

## `nh caddy remove`

Removes the import line from the master Caddyfile via `grep -vxF` to a temp
file, then atomic `mv`. No regex hazards.

By default, the project's `Caddyfile.local` is kept (might have hand-tuned
content). Pass `--purge` to also delete the file and unregister the project.

## `nh caddy reload`

Wraps `caddy reload --config <master> --address 127.0.0.1:2019`. Uses
Caddy's admin API — graceful, zero downtime. If Caddy isn't running, the
underlying command will error and surface the message naturally.

The explicit `--address 127.0.0.1:2019` is **required** on macOS: Caddy's
admin server binds only to IPv4 by default, but `localhost` resolves to
IPv6 `[::1]` first (because of the `::1 localhost` line in `/etc/hosts`),
so without the flag the reload client gets "dial tcp [::1]:2019: connect:
connection refused".

Important: Caddy does **not** watch the filesystem. After every change to
master or any `Caddyfile.local`, the user must explicitly reload.

If reload fails with "ambiguous site definition", the master Caddyfile and
some imported `Caddyfile.local` are both claiming the same hostname. Caddy
rejects the new config and keeps the previous one running — find both
definitions (grep both files), decide which owns the domain, delete the
other.

If reload fails with "connection refused", check whether Caddy is actually
running (`brew services list`, `pgrep -lf caddy`). Brew installs ship a
launchd plist; `/opt/homebrew/etc/Caddyfile` is symlinked to the master.

## Conventions and Gotchas

- **Filename is `Caddyfile.local`** — matches `caddyfile-mode` Emacs major
  mode for syntax highlighting, distinct from any production `Caddyfile`.
- **`.test` TLD** is the user's convention for local dev. Browsers route it
  to Caddy because of system-level resolution (likely dnsmasq or hosts).
- **`tls internal`** triggers Caddy's local CA — first use will prompt for
  trust; certs persist in `~/Library/Application Support/Caddy/`.
- **Conflicting hostnames fail loud at reload** — Caddy refuses the config
  and keeps the previous one running. You can't accidentally clobber.
- **Globs in `import` are evaluated at load time.** New project added but
  not reloaded yet → Caddy doesn't see it. Always finish with `nh caddy
  reload`.
- **Manual edits to master are fine** — the user has a flat master Caddyfile
  with hand-written site blocks for ~25 projects. `nh caddy new` appends
  imports below those; `nh caddy remove` only touches matching import lines,
  not the hand-written content.

## Common Tasks

### Adding a new reverse-proxied project
```
nh caddy new vane --port 3000
nh caddy reload
```
Creates `~/Projects/<org>/vane/Caddyfile.local`, appends an import to master,
auto-registers, then reloads. The site is live at `vane.test`.

### Adding a static-file project
```
nh caddy new docs --static
nh caddy reload
```

### Listing what's currently wired up
```
nh caddy list
nh caddy list --paths   # include filesystem paths
```

### Tearing down a project
```
nh caddy remove vane --purge   # also deletes Caddyfile.local
nh caddy reload
```

### Just disable an import (keep the file)
```
nh caddy remove vane           # leaves Caddyfile.local intact
nh caddy reload
```
Re-adding later: `nh caddy new vane --port 3000` will error because the file
still exists — use `nh caddy remove vane --purge` first, or just edit the
existing `Caddyfile.local` directly and re-add the import manually.

## Key Files

- `dot-config/fish/functions/_nh_caddy.fish` — main dispatcher and all
  caddy subcommand implementations (`_nh_caddy`, `_nh_caddy_new`,
  `_nh_caddy_remove`, `_nh_caddy_list`, `_nh_caddy_reload`,
  `_nh_caddy_target`, `_nh_caddy_master`, `_nh_caddy_help`).
- `dot-config/fish/functions/_nh_project_resolve.fish` — project name → path.
- `dot-config/fish/functions/_nh_project_register.fish` — adds to registry.
- `dot-config/fish/functions/_nh_project_unregister.fish` — removes from
  registry.
- `dot-config/fish/functions/_nh_project_registry_file.fish` — returns
  registry path.
- `dot-config/fish/completions/nh.fish` — completions for `nh caddy *` and
  flags. Project-name completion calls `nh projects` to enumerate.
- `caddy/Caddyfile` (gitignored, per-machine) — the master file.
- `~/.local/state/nh/projects` — registry file (`name=path` lines).

## Fish Autoloading Note

The `_nh_project_*` helpers are split into per-function files (not bundled)
because fish autoloads functions by filename match. A bundled file like
`_nh_project.fish` would never autoload — no function named `_nh_project`
exists to trigger it. Always split helpers that are called from *outside*
their own module into per-function files.

The `_nh_caddy_*` helpers can stay bundled in `_nh_caddy.fish` because
they're only called from within `_nh_caddy`, which autoloads the file first.

## Related Skills

- `nh` — overview of the whole `nh` CLI.
- `nh-sync` — package reconciliation (separate concern).
