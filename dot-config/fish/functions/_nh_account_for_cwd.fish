# Detect the Claude Code account for the current working directory.
#
# Looks at cwd; if it's under ~/Projects/, consults the
# ~/.local/state/nh/account-orgs registry (lines like `<path>=<account>`)
# to find which account owns the closest matching path.
#
# Registry LHS forms:
#   <org>              matches all repos under ~/Projects/<org>/
#   <org>/<repo>       matches that specific repo (and its subtree)
#
# When multiple registry entries match, the LONGEST LHS wins (most specific).
#
# Echoes the account name on a hit. On miss (no registry, cwd outside
# ~/Projects/, or no matching path), echoes nothing — caller should fall
# back to the default account (see _nh_claude).

function _nh_account_for_cwd
    set -l cwd (pwd)
    set -l projects_root "$HOME/Projects"

    if not string match -q "$projects_root/*" -- $cwd
        return 0
    end

    set -l relative (string replace "$projects_root/" "" -- $cwd)

    if test -z "$relative"
        return 0
    end

    set -l registry (_nh_state_dir)/account-orgs
    if not test -f $registry
        return 0
    end

    set -l best_len 0
    set -l best_account ""

    while read -l line
        set line (string trim $line)
        if test -z "$line"; or string match -q "#*" -- $line
            continue
        end

        set -l lhs (echo $line | string replace -r '=.*' '')
        set -l rhs (echo $line | string replace -r '^[^=]*=' '')

        if test -z "$lhs"; or test -z "$rhs"
            continue
        end

        # Match if relative equals lhs OR starts with "lhs/".
        if test "$relative" = "$lhs"; or string match -q "$lhs/*" -- $relative
            set -l lhs_len (string length -- $lhs)
            if test $lhs_len -gt $best_len
                set best_len $lhs_len
                set best_account $rhs
            end
        end
    end <$registry

    if test -n "$best_account"
        echo $best_account
    end
end
