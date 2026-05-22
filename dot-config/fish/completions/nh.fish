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
complete -c nh -n "__fish_seen_subcommand_from claude; and not __fish_seen_subcommand_from continue new" -a continue -d "Resume the most recent conversation (default)"
complete -c nh -n "__fish_seen_subcommand_from claude; and not __fish_seen_subcommand_from continue new" -a new -d "Start a fresh conversation"

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
