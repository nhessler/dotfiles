# Formats a date string as a human-readable "X days ago" string

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
