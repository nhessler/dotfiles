# Ensures the nh state directory exists

function _nh_ensure_state_dir
    set -l dir (_nh_state_dir)
    if not test -d $dir
        mkdir -p $dir
    end
end
