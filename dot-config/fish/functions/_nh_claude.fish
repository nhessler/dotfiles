# nh claude — multi-account Claude Code launcher
#
# Launch:
#   nh claude              Resume the most recent conversation (default)
#   nh claude new          Start a fresh conversation
#   nh claude continue     Explicit form of the default
#
# Configuration:
#   nh claude current      Show the active account for the current directory
#   nh claude register <account>     Register a new account from dot-claude.d/<account>/
#   nh claude unregister <account>   Unregister an account (does not delete its dot-claude.d dir)
#   nh claude map <path> <account>   Route ~/Projects/<path>/ to <account>
#   nh claude unmap <path>           Remove a mapping
#   nh claude list         Show all accounts and the orgs that route to each
#
# CLAUDE_CONFIG_DIR is always set to ~/.claude.d/<account> at launch.

function _nh_claude_dotfiles_root
    echo "$HOME/Projects/nhessler/dotfiles"
end

function _nh_claude_in_multi_mode
    test -d "$HOME/.claude.d"
end

function _nh_claude_known_accounts
    if _nh_claude_in_multi_mode
        for entry in $HOME/.claude.d/*
            echo (basename $entry)
        end
    else if test -L "$HOME/.claude"
        echo nh
    end
end

function _nh_claude_account_known -a account
    if test "$account" = nh
        return 0
    end

    if test -e "$HOME/.claude.d/$account"
        return 0
    end

    return 1
end

function _nh_claude_help
    echo "Usage: nh claude [command] [args]"
    echo ""
    echo "Multi-account Claude Code launcher with cwd-based auto-routing."
    echo ""
    echo "Launch commands:"
    echo "  (none)              Resume the most recent conversation"
    echo "  new                 Start a fresh conversation"
    echo "  continue            Explicit form of the default"
    echo ""
    echo "Configuration commands:"
    echo "  current                 Show the active account for the current directory"
    echo "  register <account>      Register an account from dot-claude.d/<account>/"
    echo "  unregister <account>    Unregister an account"
    echo "  map <path> <account>    Route ~/Projects/<path>/ to an account"
    echo "  unmap <path>            Remove a mapping"
    echo "  list                    Show all accounts and the orgs that route to each"
    echo "  help                    Show this help message"
    echo ""
    echo "Routing:"
    echo "  Auto-detected from cwd via ~/.local/state/nh/account-orgs."
    echo "  Longest matching path wins. Default account when nothing matches: nh."
    echo "  CLAUDE_CONFIG_DIR is always set to ~/.claude.d/<account>."
end

function _nh_claude -d "Open Claude Code in a named Ghostty tab"
    set -l subcmd $argv[1]
    set -l args $argv[2..-1]

    # Configuration subcommands — these don't launch claude.
    switch "$subcmd"
        case current
            _nh_claude_current $args
            return $status
        case register
            _nh_claude_register $args
            return $status
        case unregister
            _nh_claude_unregister $args
            return $status
        case map
            _nh_claude_map $args
            return $status
        case unmap
            _nh_claude_unmap $args
            return $status
        case list ls
            _nh_claude_list $args
            return $status
        case help -h --help
            _nh_claude_help
            return 0
    end

    # Launch path.
    set -l folder_name (basename (pwd))

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
            _nh_claude_help
            return 1
    end

    env CLAUDE_CONFIG_DIR=$config_dir claude $flags
end

# ─── Migration ──────────────────────────────────────────────────────────────

function _nh_claude_migrate
    # Convention → multi-account mode.
    set -l dotfiles (_nh_claude_dotfiles_root)
    set -l link "$HOME/.claude"
    set -l multi "$HOME/.claude.d"

    if test -e $multi
        echo "Migration aborted: $multi already exists" >&2
        return 1
    end

    if not test -L $link
        echo "Migration aborted: $link is not a symlink (unexpected state)" >&2
        return 1
    end

    rm $link
    mkdir $multi
    ln -s "$dotfiles/dot-claude" "$multi/nh"
    echo "Migrated to multi-account mode: $multi/nh → $dotfiles/dot-claude"
end

function _nh_claude_demigrate
    # Multi-account mode → convention. Only safe when only `nh` remains.
    set -l dotfiles (_nh_claude_dotfiles_root)
    set -l link "$HOME/.claude"
    set -l multi "$HOME/.claude.d"

    set -l entries $multi/*
    if test (count $entries) -ne 1
        echo "Demigration skipped: $multi has "(count $entries)" entries (expected 1)" >&2
        return 1
    end

    if test (basename $entries[1]) != nh
        echo "Demigration skipped: lone entry is "(basename $entries[1])", not nh" >&2
        return 1
    end

    if test -e $link
        echo "Demigration aborted: $link already exists" >&2
        return 1
    end

    rm "$multi/nh"
    rmdir $multi
    ln -s "$dotfiles/dot-claude" $link
    echo "Reverted to convention mode: $link → $dotfiles/dot-claude"
end

# ─── current / list formatting ──────────────────────────────────────────────

# Compute display values (symlink path, target path) for an account.
# Echoes "symlink<TAB>target" with $HOME and dotfiles-root prefixes abbreviated.
function _nh_claude_paths -a name
    set -l dotfiles (_nh_claude_dotfiles_root)
    set -l link

    if _nh_claude_in_multi_mode
        set link "$HOME/.claude.d/$name"
    else if test "$name" = nh
        set link "$HOME/.claude"
    else
        set link "$HOME/.claude.d/$name"
    end

    set -l target ""
    if test -L $link
        set target (readlink $link | string replace -r '/$' '')
    end

    set -l link_disp (string replace "$HOME" '~' -- $link)
    set -l target_disp $target
    if test -n "$target"
        set target_disp (string replace "$dotfiles/" '' -- $target)
    else
        set target_disp "(not linked)"
    end

    echo $link_disp\t$target_disp
end

# Compute column widths for a list of accounts. Echoes "max_name<TAB>max_link".
function _nh_claude_compute_widths
    set -l names $argv
    set -l max_name 0
    set -l max_link 0

    for n in $names
        set -l parts (string split \t -- (_nh_claude_paths $n))
        set -l nl (string length -- $n)
        set -l ll (string length -- $parts[1])
        if test $nl -gt $max_name
            set max_name $nl
        end
        if test $ll -gt $max_link
            set max_link $ll
        end
    end

    echo $max_name\t$max_link
end

# Print one account's row (account → symlink → target [(default)]) using the
# given column widths.
function _nh_claude_print_row -a name max_name max_link
    set -l parts (string split \t -- (_nh_claude_paths $name))
    set -l tag ""
    if test "$name" = nh
        set tag "  (default)"
    end
    printf "%-*s  →  %-*s  →  %s%s\n" $max_name $name $max_link $parts[1] $parts[2] $tag
end

function _nh_claude_current
    set -l account (_nh_account_for_cwd)
    if test -z "$account"
        set account nh
    end

    set -l widths (string split \t -- (_nh_claude_compute_widths $account))
    _nh_claude_print_row $account $widths[1] $widths[2]
end

function _nh_claude_list
    set -l accounts (_nh_claude_known_accounts)
    if test (count $accounts) -eq 0
        echo "No accounts configured." >&2
        return 1
    end

    set -l registry (_nh_state_dir)/account-orgs
    set -l widths (string split \t -- (_nh_claude_compute_widths $accounts))
    set -l max_name $widths[1]
    set -l max_link $widths[2]

    for account in $accounts
        _nh_claude_print_row $account $max_name $max_link
        _nh_claude_print_orgs_for $account $registry
        if test "$account" = nh
            echo "    (everything else)"
        end
    end
end

# ─── register / unregister ──────────────────────────────────────────────────

function _nh_claude_register -a name
    if test -z "$name"
        echo "Usage: nh claude register <account>" >&2
        return 1
    end

    if test "$name" = nh
        echo "'nh' is the default account and is auto-managed." >&2
        return 1
    end

    set -l dotfiles (_nh_claude_dotfiles_root)
    set -l src "$dotfiles/dot-claude.d/$name"
    set -l multi "$HOME/.claude.d"
    set -l target "$multi/$name"

    if not test -d $src
        echo "Account source $src does not exist." >&2
        echo "Create it first (mkdir $src) and re-run." >&2
        return 1
    end

    if _nh_claude_in_multi_mode
        if test -e $target
            echo "Account '$name' is already registered ($target exists)." >&2
            return 1
        end
    else
        _nh_claude_migrate; or return 1
    end

    ln -s $src $target
    echo "Registered account '$name': $target → $src"
end

function _nh_claude_unregister -a name
    if test -z "$name"
        echo "Usage: nh claude unregister <account>" >&2
        return 1
    end

    if test "$name" = nh
        echo "'nh' is the default account and cannot be unregistered." >&2
        return 1
    end

    if not _nh_claude_in_multi_mode
        echo "Not in multi-account mode (~/.claude.d/ does not exist)." >&2
        return 1
    end

    set -l multi "$HOME/.claude.d"
    set -l target "$multi/$name"

    if not test -e $target
        echo "Account '$name' is not registered ($target does not exist)." >&2
        return 1
    end

    # Drop any org mappings pointing to this account.
    set -l registry (_nh_state_dir)/account-orgs
    set -l dropped 0
    if test -f $registry
        set -l tmp (mktemp)
        grep -v "=$name\$" $registry > $tmp
        set -l before (wc -l <$registry | string trim)
        set -l after (wc -l <$tmp | string trim)
        set dropped (math $before - $after)
        mv $tmp $registry
    end

    rm $target
    echo "Unregistered account '$name'"
    if test $dropped -gt 0
        echo "  (dropped $dropped org mapping(s) pointing to '$name')"
    end

    # Demigrate if only nh remains.
    set -l remaining $multi/*
    if test (count $remaining) -eq 1; and test (basename $remaining[1]) = nh
        _nh_claude_demigrate
    end
end

# ─── map / unmap ────────────────────────────────────────────────────────────

function _nh_claude_map -a path account
    if test -z "$path"; or test -z "$account"
        echo "Usage: nh claude map <path> <account>" >&2
        return 1
    end

    if string match -q '/*' -- $path
        echo "Absolute paths are not supported; use <org> or <org>/<repo> relative to ~/Projects/" >&2
        return 1
    end

    set path (string trim --chars=/ -- $path)

    if not _nh_claude_account_known $account
        echo "Account '$account' is not registered." >&2
        echo "Register it with: nh claude register $account" >&2
        return 1
    end

    _nh_ensure_state_dir
    set -l registry (_nh_state_dir)/account-orgs

    if test -f $registry; and grep -q "^$path=" $registry 2>/dev/null
        set -l existing (grep "^$path=" $registry | head -n1 | string replace -r '^[^=]*=' '')
        echo "'$path' is already mapped to account '$existing'." >&2
        echo "Run: nh claude unmap $path" >&2
        return 1
    end

    echo "$path=$account" >> $registry
    echo "Mapped $path → $account"
end

function _nh_claude_unmap -a path
    if test -z "$path"
        echo "Usage: nh claude unmap <path>" >&2
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

# ─── list helpers (org rows under each account, with exception annotation) ──

# Print indented org rows for $account, annotating any exceptions inline.
function _nh_claude_print_orgs_for -a account registry
    if not test -f $registry
        return 0
    end

    while read -l line
        set line (string trim $line)
        if test -z "$line"; or string match -q "#*" -- $line
            continue
        end
        set -l lhs (echo $line | string replace -r '=.*' '')
        set -l rhs (echo $line | string replace -r '^[^=]*=' '')
        if test "$rhs" = "$account"
            set -l excs (_nh_claude_direct_exceptions $lhs $registry)
            if test -n "$excs"
                echo "    $lhs (except $excs)"
            else
                echo "    $lhs"
            end
        end
    end <$registry
end

# Return a comma-joined list of direct exceptions to $path:
# paths P' such that $path is the longest mapped prefix of P' and P''s
# account differs from $path's account.
function _nh_claude_direct_exceptions -a path registry
    set -l path_account (grep "^$path=" $registry | head -n1 | string replace -r '^[^=]*=' '')

    set -l exceptions
    while read -l line
        set line (string trim $line)
        if test -z "$line"; or string match -q "#*" -- $line
            continue
        end
        set -l candidate (echo $line | string replace -r '=.*' '')
        set -l candidate_account (echo $line | string replace -r '^[^=]*=' '')

        if not string match -q "$path/*" -- $candidate
            continue
        end
        if test "$candidate_account" = "$path_account"
            continue
        end

        set -l longest (_nh_claude_longest_prefix $candidate $registry)
        if test "$longest" = "$path"
            set -a exceptions $candidate
        end
    end <$registry

    if test (count $exceptions) -gt 0
        string join ", " $exceptions
    end
end

# Return the longest mapped path that is a strict prefix of $path.
function _nh_claude_longest_prefix -a path registry
    set -l best ""
    set -l best_len 0

    while read -l line
        set line (string trim $line)
        if test -z "$line"; or string match -q "#*" -- $line
            continue
        end
        set -l lhs (echo $line | string replace -r '=.*' '')
        if test "$path" != "$lhs"; and string match -q "$lhs/*" -- $path
            set -l l (string length -- $lhs)
            if test $l -gt $best_len
                set best $lhs
                set best_len $l
            end
        end
    end <$registry

    echo $best
end
