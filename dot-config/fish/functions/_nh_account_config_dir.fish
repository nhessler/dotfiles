# Resolve a Claude Code account name to its CLAUDE_CONFIG_DIR path.
#
# Convention:
#   <name> → ~/.claude.d/<name>
#
# Override convention (optional): an entry in ~/.local/state/nh/accounts
# with `<name>=<absolute-path>` takes precedence over the default
# ~/.claude.d/<name>.

function _nh_account_config_dir -a name
    if test -z "$name"
        echo "_nh_account_config_dir: name required" >&2
        return 2
    end

    set -l registry (_nh_state_dir)/accounts
    if test -f $registry
        set -l hit (grep "^$name=" $registry | head -n1 | string replace -r '^[^=]+=' '')
        if test -n "$hit"
            echo $hit
            return 0
        end
    end

    echo "$HOME/.claude.d/$name"
end
