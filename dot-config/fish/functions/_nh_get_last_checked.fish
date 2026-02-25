# Gets the last-checked timestamp for an outdated category

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
