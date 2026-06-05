# nh claude account subcommand
#
#   nh claude account                  Show the active account for cwd
#   nh claude account add <name>       Symlink ~/.claude.d/<name> → dot-claude.d/<name>/
#   nh claude account remove <name>    Unlink ~/.claude.d/<name>; reverts to convention mode if last
#   nh claude account list             Show all accounts and their org mappings
#
# The default account `nh` is auto-managed and cannot be added or removed.
# It exists implicitly via dot-claude/ in convention mode (~/.claude → dot-claude/)
# or explicitly via ~/.claude.d/nh → dot-claude/ in multi-account mode.
#
# Adding any non-default account migrates convention → multi. Removing the
# last non-default account demigrates multi → convention.

function _nh_claude_account_dotfiles_root
    echo "$HOME/Projects/nhessler/dotfiles"
end

function _nh_claude_account_in_multi_mode
    test -d "$HOME/.claude.d"
end

function _nh_claude_account_help
    echo "Usage: nh claude account [command] [args]"
    echo ""
    echo "Manage Claude Code accounts."
    echo ""
    echo "Commands:"
    echo "  (none)              Show the active account for the current directory"
    echo "  add <name>          Register a new account from dot-claude.d/<name>/"
    echo "  remove <name>       Unregister an account (does not delete dot-claude.d/<name>/)"
    echo "  list                Show all accounts and their org mappings"
    echo "  help                Show this help message"
    echo ""
    echo "Notes:"
    echo "  - dot-claude.d/<name>/ must exist before 'add'."
    echo "  - The default account 'nh' cannot be added or removed."
    echo "  - Adding the first non-default account migrates to multi-account mode"
    echo "    (~/.claude is replaced by ~/.claude.d/{nh,...} symlinks)."
    echo "  - Removing the last non-default account reverts to convention mode."
end

function _nh_claude_account
    set -l subcmd $argv[1]
    set -l args $argv[2..-1]

    switch "$subcmd"
        case add
            _nh_claude_account_add $args
        case remove rm
            _nh_claude_account_remove $args
        case list ls
            _nh_claude_account_list $args
        case help -h --help
            _nh_claude_account_help
        case ''
            _nh_claude_account_active
        case '*'
            echo "nh claude account: unknown command '$subcmd'" >&2
            echo ""
            _nh_claude_account_help
            return 1
    end
end

function _nh_claude_account_active
    set -l account (_nh_account_for_cwd)
    if test -z "$account"
        set account nh
    end

    _nh_claude_account_format $account
end

# Compute display values (symlink path, target path) for an account.
# Echoes "symlink<TAB>target" with $HOME and dotfiles-root prefixes abbreviated.
function _nh_claude_account_paths -a name
    set -l dotfiles (_nh_claude_account_dotfiles_root)
    set -l link

    if _nh_claude_account_in_multi_mode
        set link "$HOME/.claude.d/$name"
    else if test "$name" = nh
        set link "$HOME/.claude"
    else
        # Non-default account requested while in convention mode — paths exist
        # only in theory; report what the path WOULD be.
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

# Print one or more account rows in a 3-column arrow-aligned layout.
# Computes column widths from the inputs so columns line up.
function _nh_claude_account_format
    set -l names $argv

    set -l max_name 0
    set -l max_link 0
    set -l links
    set -l targets

    for n in $names
        set -l parts (string split \t -- (_nh_claude_account_paths $n))
        set -a links $parts[1]
        set -a targets $parts[2]

        set -l nl (string length -- $n)
        set -l ll (string length -- $parts[1])
        if test $nl -gt $max_name
            set max_name $nl
        end
        if test $ll -gt $max_link
            set max_link $ll
        end
    end

    set -l i 1
    while test $i -le (count $names)
        set -l n $names[$i]
        set -l tag ""
        if test "$n" = nh
            set tag "  (default)"
        end
        printf "%-*s  →  %-*s  →  %s%s\n" $max_name $n $max_link $links[$i] $targets[$i] $tag
        set i (math $i + 1)
    end
end

function _nh_claude_account_migrate
    # Convention → multi-account mode.
    set -l dotfiles (_nh_claude_account_dotfiles_root)
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

function _nh_claude_account_demigrate
    # Multi-account mode → convention. Only safe when only `nh` remains.
    set -l dotfiles (_nh_claude_account_dotfiles_root)
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

function _nh_claude_account_add -a name
    if test -z "$name"
        echo "Usage: nh claude account add <name>" >&2
        return 1
    end

    if test "$name" = nh
        echo "'nh' is the default account and is auto-managed." >&2
        return 1
    end

    set -l dotfiles (_nh_claude_account_dotfiles_root)
    set -l src "$dotfiles/dot-claude.d/$name"
    set -l multi "$HOME/.claude.d"
    set -l target "$multi/$name"

    if not test -d $src
        echo "Account source $src does not exist." >&2
        echo "Create it first (mkdir $src) and re-run." >&2
        return 1
    end

    if _nh_claude_account_in_multi_mode
        if test -e $target
            echo "Account '$name' is already registered ($target exists)." >&2
            return 1
        end
    else
        # Migrate first.
        _nh_claude_account_migrate; or return 1
    end

    ln -s $src $target
    echo "Registered account '$name': $target → $src"
end

function _nh_claude_account_remove -a name
    if test -z "$name"
        echo "Usage: nh claude account remove <name>" >&2
        return 1
    end

    if test "$name" = nh
        echo "'nh' is the default account and cannot be removed." >&2
        return 1
    end

    if not _nh_claude_account_in_multi_mode
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
        _nh_claude_account_demigrate
    end
end

function _nh_claude_account_list
    if _nh_claude_account_in_multi_mode
        set -l names
        for entry in $HOME/.claude.d/*
            set -a names (basename $entry)
        end
        _nh_claude_account_format $names
    else
        set -l link "$HOME/.claude"
        if test -L $link
            _nh_claude_account_format nh
            echo "(convention mode)"
        else
            echo "No accounts configured." >&2
            return 1
        end
    end
end

