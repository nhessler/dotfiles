function _nh_claude -d "Open Claude Code in a named Ghostty tab"
    set -l folder_name (basename (pwd))
    set -l subcmd $argv[1]

    switch "$subcmd"
        case new
            claude --name "$folder_name - CC"
        case continue ''
            claude --continue --name "$folder_name - CC"
        case help -h --help
            echo "Usage: nh claude [command]"
            echo ""
            echo "Open Claude Code in a named Ghostty tab."
            echo ""
            echo "Commands:"
            echo "  continue    Resume the most recent conversation (default)"
            echo "  new         Start a fresh conversation"
            echo "  help        Show this help message"
        case '*'
            echo "nh claude: unknown command '$subcmd'"
            echo ""
            _nh_claude help
            return 1
    end
end
