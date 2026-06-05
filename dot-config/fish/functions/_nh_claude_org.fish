# nh claude org subcommand
#
#   nh claude org map <path> <account>     Route cwds under ~/Projects/<path>/ to <account>
#   nh claude org unmap <path>             Remove a mapping
#   nh claude org list                     Show all mappings
#
# <path> is either an org name (e.g. `lolollc`) or org/repo (e.g. `lolollc/qrlite`).
# It's resolved relative to ~/Projects/. The longest matching path wins at
# resolution time (see _nh_account_for_cwd).
#
# Mappings live in ~/.local/state/nh/account-orgs as `<path>=<account>` lines.

function _nh_claude_org_help
    echo "Usage: nh claude org <command> [args]"
    echo ""
    echo "Manage path → account routing for Claude Code."
    echo ""
    echo "Commands:"
    echo "  map <path> <account>    Route ~/Projects/<path>/ to <account>"
    echo "  unmap <path>            Remove an existing mapping"
    echo "  list                    Show all mappings"
    echo "  help                    Show this help message"
    echo ""
    echo "Notes:"
    echo "  - <path> is org (lolollc) or org/repo (lolollc/qrlite), relative to ~/Projects/."
    echo "  - More specific paths (longer LHS) win at resolution time."
    echo "  - <account> must be registered via 'nh claude account add' (or be 'nh')."
    echo "  - Use 'unmap' before 'map' to reassign an existing path."
end

function _nh_claude_org
    set -l subcmd $argv[1]
    set -l args $argv[2..-1]

    switch "$subcmd"
        case map
            _nh_claude_org_map $args
        case unmap
            _nh_claude_org_unmap $args
        case list ls
            _nh_claude_org_list $args
        case help -h --help ''
            _nh_claude_org_help
        case '*'
            echo "nh claude org: unknown command '$subcmd'" >&2
            echo ""
            _nh_claude_org_help
            return 1
    end
end

# Check whether <account> is a known account (registered or the default).
function _nh_claude_org_account_known -a account
    if test "$account" = nh
        return 0
    end

    if test -e "$HOME/.claude.d/$account"
        return 0
    end

    return 1
end

function _nh_claude_org_map -a path account
    if test -z "$path"; or test -z "$account"
        echo "Usage: nh claude org map <path> <account>" >&2
        return 1
    end

    # Reject leading slash (absolute paths not supported yet) and trailing slash.
    if string match -q '/*' -- $path
        echo "Absolute paths are not supported; use <org> or <org>/<repo> relative to ~/Projects/" >&2
        return 1
    end

    set path (string trim --chars=/ -- $path)

    if not _nh_claude_org_account_known $account
        echo "Account '$account' is not registered." >&2
        echo "Register it with: nh claude account add $account" >&2
        return 1
    end

    _nh_ensure_state_dir
    set -l registry (_nh_state_dir)/account-orgs

    if test -f $registry; and grep -q "^$path=" $registry 2>/dev/null
        set -l existing (grep "^$path=" $registry | head -n1 | string replace -r '^[^=]*=' '')
        echo "'$path' is already mapped to account '$existing'." >&2
        echo "Run: nh claude org unmap $path" >&2
        return 1
    end

    echo "$path=$account" >> $registry
    echo "Mapped $path → $account"
end

function _nh_claude_org_unmap -a path
    if test -z "$path"
        echo "Usage: nh claude org unmap <path>" >&2
        return 1
    end

    set path (string trim --chars=/ -- $path)

    set -l registry (_nh_state_dir)/account-orgs
    if not test -f $registry
        echo "No mappings registered." >&2
        return 1
    end

    if not grep -q "^$path=" $registry
        echo "No mapping found for '$path'." >&2
        return 1
    end

    set -l tmp (mktemp)
    grep -v "^$path=" $registry > $tmp
    mv $tmp $registry
    echo "Unmapped $path"
end

function _nh_claude_org_list
    set -l registry (_nh_state_dir)/account-orgs
    set -l fallback "(everything else)"

    if not test -f $registry; or test ! -s $registry
        printf "  %s  →  nh\n" $fallback
        return 0
    end

    # Pretty-print: sort by path, align in two columns. Fallback is included
    # in width calculation so the arrows line up across explicit + fallback.
    set -l max_len (string length -- $fallback)
    while read -l line
        set line (string trim $line)
        if test -z "$line"; or string match -q "#*" -- $line
            continue
        end
        set -l lhs (echo $line | string replace -r '=.*' '')
        set -l l (string length -- $lhs)
        if test $l -gt $max_len
            set max_len $l
        end
    end <$registry

    sort $registry | while read -l line
        set line (string trim $line)
        if test -z "$line"; or string match -q "#*" -- $line
            continue
        end
        set -l lhs (echo $line | string replace -r '=.*' '')
        set -l rhs (echo $line | string replace -r '^[^=]*=' '')
        printf "  %-*s  →  %s\n" $max_len $lhs $rhs
    end

    printf "  %-*s  →  nh\n" $max_len $fallback
end
