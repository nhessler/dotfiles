function _nh_claude -d "Open Claude Code in a named Ghostty tab"
    set -l subcmd $argv[1]
    set -l args $argv[2..-1]

    # Sub-subcommands that don't launch claude itself.
    switch "$subcmd"
        case account
            _nh_claude_account $args
            return $status
        case org
            _nh_claude_org $args
            return $status
        case help -h --help
            echo "Usage: nh claude [command]"
            echo ""
            echo "Open Claude Code in a named Ghostty tab."
            echo ""
            echo "Launch commands:"
            echo "  continue    Resume the most recent conversation (default)"
            echo "  new         Start a fresh conversation"
            echo ""
            echo "Configuration commands:"
            echo "  account     Manage accounts (add/remove/list/show active)"
            echo "  org         Manage path → account routing (map/unmap/list)"
            echo "  help        Show this help message"
            echo ""
            echo "Account routing:"
            echo "  Auto-detected from cwd via ~/.local/state/nh/account-orgs."
            echo "  Projects under ~/Projects/<org>/ map to the account that"
            echo "  owns <org>; everything else defaults to the 'nh' account."
            echo "  CLAUDE_CONFIG_DIR is always set to ~/.claude.d/<account>."
            return 0
    end

    set -l folder_name (basename (pwd))

    # Auto-detect account from cwd. Falls back to "nh" if no org mapping.
    set -l account (_nh_account_for_cwd)
    if test -z "$account"
        set account nh
    end

    set -l config_dir (_nh_account_config_dir $account)
    set -l tab_name "$folder_name - CC [$account]"
    set -l flags

    echo "Using Claude account: $account ($config_dir)" >&2

    switch "$subcmd"
        case new
            set flags --name "$tab_name"
        case continue ''
            set flags --continue --name "$tab_name"
        case '*'
            echo "nh claude: unknown command '$subcmd'"
            echo ""
            _nh_claude help
            return 1
    end

    env CLAUDE_CONFIG_DIR=$config_dir claude $flags
end
