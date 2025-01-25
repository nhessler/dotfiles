# disable file completion
complete -c git-secure -f

complete -c git-secure -s h -l help  -rd "get help on how to use this command"
complete -c git-secure -s n -l name  -rd "name to be used for commits"
complete -c git-secure -s e -l email -rd "email to be used for commits"
complete -c git-secure -s k -l key   -rd "signing key to be used for commits"


