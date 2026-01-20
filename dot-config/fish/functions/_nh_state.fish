# Shared state helpers for nh CLI

function _nh_state_dir
    echo "$HOME/.local/state/nh"
end

function _nh_ensure_state_dir
    set -l dir (_nh_state_dir)
    if not test -d $dir
        mkdir -p $dir
    end
end

function _nh_get_last_checked -a category
    set -l state_file (_nh_state_dir)/outdated-checks

    if not test -f $state_file
        echo "never checked"
        return
    end

    set -l timestamp (grep "^$category=" $state_file 2>/dev/null | cut -d= -f2)

    if test -z "$timestamp"
        echo "never checked"
        return
    end

    _nh_format_days_ago $timestamp
end

function _nh_set_last_checked -a category
    _nh_ensure_state_dir
    set -l state_file (_nh_state_dir)/outdated-checks
    set -l today (date +%Y-%m-%d)

    if not test -f $state_file
        echo "$category=$today" > $state_file
        return
    end

    # Update or add the category
    if grep -q "^$category=" $state_file 2>/dev/null
        # Use sed to update in place
        sed -i '' "s/^$category=.*/$category=$today/" $state_file
    else
        echo "$category=$today" >> $state_file
    end
end

function _nh_format_days_ago -a date_str
    # Calculate days between date_str and today
    set -l then (date -j -f "%Y-%m-%d" "$date_str" "+%s" 2>/dev/null)
    set -l now (date "+%s")

    if test -z "$then"
        echo "checked $date_str"
        return
    end

    set -l diff (math "($now - $then) / 86400")

    if test $diff -eq 0
        echo "checked today"
    else if test $diff -eq 1
        echo "checked yesterday"
    else if test $diff -lt 7
        echo "checked $diff days ago"
    else if test $diff -lt 30
        set -l weeks (math "floor($diff / 7)")
        if test $weeks -eq 1
            echo "checked 1 week ago"
        else
            echo "checked $weeks weeks ago"
        end
    else
        set -l months (math "floor($diff / 30)")
        if test $months -eq 1
            echo "checked 1 month ago"
        else
            echo "checked $months months ago"
        end
    end
end
