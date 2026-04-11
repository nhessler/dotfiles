# Gets the last-upgraded timestamp for an upgrade category

function _nh_get_last_upgraded -a category
    set -l state_file (_nh_state_dir)/upgrade-runs

    if not test -f $state_file
        echo "never upgraded"
        return
    end

    set -l timestamp (grep "^$category=" $state_file 2>/dev/null | cut -d= -f2)

    if test -z "$timestamp"
        echo "never upgraded"
        return
    end

    echo "upgraded "(_nh_format_days_ago $timestamp)
end
