# Disable file completions by default
complete -c git-secure -f

# Options
complete -c git-secure -s h -l help -d "Show help message"
complete -c git-secure -s n -l name -r -d "Author name (default: global git user.name)"
complete -c git-secure -s e -l email -r -d "Author email for commits"
complete -c git-secure -s k -l key -rF -d "Path to private signing key"
complete -c git-secure -s f -l format -ra "ssh gpg" -d "Signature format (default: ssh)"

- -r means the option requires an argument





