function _nh_sync -d "Interactively reconcile installed vs tracked packages"
    if contains -- --help $argv; or contains -- -h $argv
        echo "Usage: nh sync [--remove] [--reset]"
        echo ""
        echo "Reconcile installed packages vs tracked config files."
        echo ""
        echo "Modes:"
        echo "  (default)   Find installed-but-not-tracked items (keep/skip)"
        echo "  --remove    Find tracked-but-not-installed items (remove/keep)"
        echo "  --reset     Clear skip list, then run default sync"
        echo ""
        echo "Categories checked: brew taps, formulae, casks, MAS, ASDF, Emacs"
        return 0
    end

    set -l mode keep
    if contains -- --remove $argv
        set mode remove
    else if contains -- --reset $argv
        _nh_sync_clear_skips
        echo "Skip list cleared."
        echo ""
    end

    _nh_ensure_state_dir

    echo "=== Brew Taps ==="
    echo "  (not yet implemented)"
    echo ""
    echo "=== Brew Formulae ==="
    echo "  (not yet implemented)"
    echo ""
    echo "=== Brew Casks ==="
    echo "  (not yet implemented)"
    echo ""
    echo "=== Mac App Store ==="
    echo "  (not yet implemented)"
    echo ""
    echo "=== ASDF Plugins ==="
    echo "  (not yet implemented)"
    echo ""
    echo "=== Emacs Packages (informational) ==="
    echo "  (not yet implemented)"
    echo ""

    _nh_sync_show_skips
end

# --- Skip list helpers ---

function _nh_sync_skip_file
    echo (_nh_state_dir)/sync-skips
end

function _nh_sync_is_skipped
    set -l type $argv[1]
    set -l item $argv[2]
    set -l skip_file (_nh_sync_skip_file)
    test -f $skip_file; and grep -qxF "$type:$item" $skip_file
end

function _nh_sync_add_skip
    set -l type $argv[1]
    set -l item $argv[2]
    echo "$type:$item" >> (_nh_sync_skip_file)
end

function _nh_sync_clear_skips
    set -l skip_file (_nh_sync_skip_file)
    if test -f $skip_file
        rm $skip_file
    end
end

function _nh_sync_show_skips
    set -l skip_file (_nh_sync_skip_file)
    if not test -f $skip_file; or test (wc -l < $skip_file | string trim) -eq 0
        return 0
    end

    echo "=== Skipped Items ==="
    for line in (cat $skip_file | sort)
        set -l type (echo $line | string split ':')[1]
        set -l item (echo $line | string split ':')[2]
        echo "  $type: $item"
    end
    echo ""
    echo "  To re-prompt for these, run: nh sync --reset"
    echo ""
end

# --- Interactive prompt helpers ---

function _nh_sync_prompt_keep
    set -l item $argv[1]
    read -P "  $item [k]eep / [s]kip / [q]uit? " -l answer
    switch (string lower $answer)
        case '' k
            echo keep
        case s
            echo skip
        case q
            echo quit
        case '*'
            echo skip
    end
end

function _nh_sync_prompt_remove
    set -l item $argv[1]
    read -P "  $item [r]emove / [k]eep / [q]uit? " -l answer
    switch (string lower $answer)
        case '' r
            echo remove
        case k
            echo keep
        case q
            echo quit
        case '*'
            echo keep
    end
end

# --- Brewfile helpers ---

function _nh_sync_brewfile_path
    echo "$HOME/.config/homebrew/Brewfile"
end
