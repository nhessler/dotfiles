# Sets the last-checked timestamp for an outdated category

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
