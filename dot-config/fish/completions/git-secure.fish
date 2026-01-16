# Disable file completions by default
complete -c git-secure -f

# Options
complete -c git-secure -s h -l help -d "Show help message"
complete -c git-secure -s n -l name -r -d "Author name (default: global git user.name)"
complete -c git-secure -s e -l email -r -d "Author email for commits"
complete -c git-secure -s k -l key -rF -d "Path to private signing key"
complete -c git-secure -s f -l format -ra "ssh gpg" -d "Signature format (default: ssh)"

# Completions for git secure (subcommand)
complete -c git -n "__fish_seen_subcommand_from secure" -s h -l help -d "Show help message"
complete -c git -n "__fish_seen_subcommand_from secure" -s n -l name -r -d "Author name (default: global git user.name)"
complete -c git -n "__fish_seen_subcommand_from secure" -s e -l email -r -d "Author email for commits"
complete -c git -n "__fish_seen_subcommand_from secure" -s k -l key -rF -d "Path to private signing key"
complete -c git -n "__fish_seen_subcommand_from secure" -s f -l format -ra "ssh gpg" -d "Signature format (default: ssh)"






