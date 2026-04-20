# Completions for nh command
complete -c nh -f

# Subcommands (only when no subcommand given yet)
complete -c nh -n "not __fish_seen_subcommand_from setup outdated upgrade sync add-keys claude help" -a setup -d "Run the dotfiles setup script"
complete -c nh -n "not __fish_seen_subcommand_from setup outdated upgrade sync add-keys claude help" -a outdated -d "Check for outdated dependencies"
complete -c nh -n "not __fish_seen_subcommand_from setup outdated upgrade sync add-keys claude help" -a upgrade -d "Upgrade brew, App Store, and asdf globals"
complete -c nh -n "not __fish_seen_subcommand_from setup outdated upgrade sync add-keys claude help" -a sync -d "Reconcile installed vs tracked packages"
complete -c nh -n "__fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from keep remove skipped reset" -a keep -d "Find installed-but-not-tracked items (default)"
complete -c nh -n "__fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from keep remove skipped reset" -a remove -d "Find tracked-but-not-installed items"
complete -c nh -n "__fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from keep remove skipped reset" -a skipped -d "Show currently skipped items"
complete -c nh -n "__fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from keep remove skipped reset" -a reset -d "Clear skip list"
complete -c nh -n "__fish_seen_subcommand_from sync" -s o -l only -xa "brew cask mas asdf emacs" -d "Only run one category"
complete -c nh -n "not __fish_seen_subcommand_from setup outdated upgrade sync add-keys claude help" -a add-keys -d "Add SSH keys to agent using keychain"
complete -c nh -n "not __fish_seen_subcommand_from setup outdated upgrade sync add-keys claude help" -a claude -d "Open Claude Code in a named tab"
complete -c nh -n "__fish_seen_subcommand_from claude; and not __fish_seen_subcommand_from continue new" -a continue -d "Resume the most recent conversation (default)"
complete -c nh -n "__fish_seen_subcommand_from claude; and not __fish_seen_subcommand_from continue new" -a new -d "Start a fresh conversation"
complete -c nh -n "not __fish_seen_subcommand_from setup outdated upgrade sync add-keys claude help" -a help -d "Show help message"
