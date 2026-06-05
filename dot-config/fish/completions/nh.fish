# Completions for nh command
complete -c nh -f

set -l top_cmds setup outdated upgrade sync add-keys claude caddy projects help

# Top-level subcommands
complete -c nh -n "not __fish_seen_subcommand_from $top_cmds" -a setup -d "Run the dotfiles setup script"
complete -c nh -n "not __fish_seen_subcommand_from $top_cmds" -a outdated -d "Check for outdated dependencies"
complete -c nh -n "not __fish_seen_subcommand_from $top_cmds" -a upgrade -d "Upgrade brew, App Store, and asdf globals"
complete -c nh -n "not __fish_seen_subcommand_from $top_cmds" -a sync -d "Reconcile installed vs tracked packages"
complete -c nh -n "not __fish_seen_subcommand_from $top_cmds" -a add-keys -d "Add SSH keys to agent using keychain"
complete -c nh -n "not __fish_seen_subcommand_from $top_cmds" -a claude -d "Open Claude Code in a named tab"
complete -c nh -n "not __fish_seen_subcommand_from $top_cmds" -a caddy -d "Manage per-project Caddyfile.local imports"
complete -c nh -n "not __fish_seen_subcommand_from $top_cmds" -a projects -d "List projects under ~/Projects/<org>/<name>"
complete -c nh -n "not __fish_seen_subcommand_from $top_cmds" -a help -d "Show help message"

# sync sub-subcommands
complete -c nh -n "__fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from keep remove skipped reset" -a keep -d "Find installed-but-not-tracked items (default)"
complete -c nh -n "__fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from keep remove skipped reset" -a remove -d "Find tracked-but-not-installed items"
complete -c nh -n "__fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from keep remove skipped reset" -a skipped -d "Show currently skipped items"
complete -c nh -n "__fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from keep remove skipped reset" -a reset -d "Clear skip list"
complete -c nh -n "__fish_seen_subcommand_from sync" -s o -l only -xa "brew cask mas asdf emacs" -d "Only run one category"

# claude sub-subcommands
set -l claude_subs continue new account org help
complete -c nh -n "__fish_seen_subcommand_from claude; and not __fish_seen_subcommand_from $claude_subs" -a continue -d "Resume the most recent conversation (default)"
complete -c nh -n "__fish_seen_subcommand_from claude; and not __fish_seen_subcommand_from $claude_subs" -a new -d "Start a fresh conversation"
complete -c nh -n "__fish_seen_subcommand_from claude; and not __fish_seen_subcommand_from $claude_subs" -a account -d "Manage Claude Code accounts"
complete -c nh -n "__fish_seen_subcommand_from claude; and not __fish_seen_subcommand_from $claude_subs" -a org -d "Manage path → account routing"
complete -c nh -n "__fish_seen_subcommand_from claude; and not __fish_seen_subcommand_from $claude_subs" -a help -d "Show help message"

# claude account sub-sub-subcommands
set -l acct_subs add remove rm list ls help
complete -c nh -n "__fish_seen_subcommand_from claude; and __fish_seen_subcommand_from account; and not __fish_seen_subcommand_from $acct_subs" -a add -d "Register an account from dot-claude.d/<name>/"
complete -c nh -n "__fish_seen_subcommand_from claude; and __fish_seen_subcommand_from account; and not __fish_seen_subcommand_from $acct_subs" -a remove -d "Unregister an account"
complete -c nh -n "__fish_seen_subcommand_from claude; and __fish_seen_subcommand_from account; and not __fish_seen_subcommand_from $acct_subs" -a list -d "Show all accounts and their org mappings"
complete -c nh -n "__fish_seen_subcommand_from claude; and __fish_seen_subcommand_from account; and not __fish_seen_subcommand_from $acct_subs" -a help -d "Show account help"

# claude account add — complete from dot-claude.d/ entries (those not yet linked)
complete -c nh -n "__fish_seen_subcommand_from claude; and __fish_seen_subcommand_from account; and __fish_seen_subcommand_from add" -a "(command ls ~/Projects/nhessler/dotfiles/dot-claude.d 2>/dev/null)" -d "available account"

# claude account remove — complete from currently-registered accounts (excluding nh)
complete -c nh -n "__fish_seen_subcommand_from claude; and __fish_seen_subcommand_from account; and __fish_seen_subcommand_from remove rm" -a "(command ls ~/.claude.d 2>/dev/null | grep -v '^nh\$')" -d "registered account"

# claude org sub-sub-subcommands
set -l org_subs map unmap list ls help
complete -c nh -n "__fish_seen_subcommand_from claude; and __fish_seen_subcommand_from org; and not __fish_seen_subcommand_from $org_subs" -a map -d "Route ~/Projects/<path>/ to an account"
complete -c nh -n "__fish_seen_subcommand_from claude; and __fish_seen_subcommand_from org; and not __fish_seen_subcommand_from $org_subs" -a unmap -d "Remove a mapping"
complete -c nh -n "__fish_seen_subcommand_from claude; and __fish_seen_subcommand_from org; and not __fish_seen_subcommand_from $org_subs" -a list -d "Show all mappings"
complete -c nh -n "__fish_seen_subcommand_from claude; and __fish_seen_subcommand_from org; and not __fish_seen_subcommand_from $org_subs" -a help -d "Show org help"

# claude org unmap — complete from existing mappings
complete -c nh -n "__fish_seen_subcommand_from claude; and __fish_seen_subcommand_from org; and __fish_seen_subcommand_from unmap" -a "(cut -d= -f1 ~/.local/state/nh/account-orgs 2>/dev/null)" -d "mapped path"

# caddy sub-subcommands
set -l caddy_subs new remove rm list ls reload help
complete -c nh -n "__fish_seen_subcommand_from caddy; and not __fish_seen_subcommand_from $caddy_subs" -a new -d "Create Caddyfile.local and add import to master"
complete -c nh -n "__fish_seen_subcommand_from caddy; and not __fish_seen_subcommand_from $caddy_subs" -a remove -d "Remove import from master (use --purge to also delete file)"
complete -c nh -n "__fish_seen_subcommand_from caddy; and not __fish_seen_subcommand_from $caddy_subs" -a list -d "List projects currently imported into the master Caddyfile"
complete -c nh -n "__fish_seen_subcommand_from caddy; and not __fish_seen_subcommand_from $caddy_subs" -a reload -d "Gracefully reload the running Caddy server"
complete -c nh -n "__fish_seen_subcommand_from caddy; and not __fish_seen_subcommand_from $caddy_subs" -a help -d "Show caddy command help"

# caddy new/remove project name completion + flags
complete -c nh -n "__fish_seen_subcommand_from caddy; and __fish_seen_subcommand_from new remove rm" -a "(nh projects)" -d "project"
complete -c nh -n "__fish_seen_subcommand_from caddy; and __fish_seen_subcommand_from new" -l domain -x -d "Domain to serve"
complete -c nh -n "__fish_seen_subcommand_from caddy; and __fish_seen_subcommand_from new" -l port -x -d "Reverse-proxy target port"
complete -c nh -n "__fish_seen_subcommand_from caddy; and __fish_seen_subcommand_from new" -l static -d "Generate a file_server template"
complete -c nh -n "__fish_seen_subcommand_from caddy; and __fish_seen_subcommand_from remove rm" -l purge -d "Also delete the Caddyfile.local"
complete -c nh -n "__fish_seen_subcommand_from caddy; and __fish_seen_subcommand_from list ls" -l paths -d "Include the Caddyfile.local path"

# projects org completion
complete -c nh -n "__fish_seen_subcommand_from projects" -a "(command ls ~/Projects 2>/dev/null)" -d "org"
complete -c nh -n "__fish_seen_subcommand_from projects" -l paths -d "Include full filesystem paths"
