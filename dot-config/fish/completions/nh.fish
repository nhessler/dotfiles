# Completions for nh command
complete -c nh -f

# Subcommands (only when no subcommand given yet)
complete -c nh -n "not __fish_seen_subcommand_from setup outdated add-keys help" -a setup -d "Run the dotfiles setup script"
complete -c nh -n "not __fish_seen_subcommand_from setup outdated add-keys help" -a outdated -d "Check for outdated dependencies"
complete -c nh -n "not __fish_seen_subcommand_from setup outdated add-keys help" -a add-keys -d "Add SSH keys to agent using keychain"
complete -c nh -n "not __fish_seen_subcommand_from setup outdated add-keys help" -a help -d "Show help message"
