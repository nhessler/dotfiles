function _nh_upgrade -d "Upgrade system packages and set asdf globals to latest installed"
    if contains -- --help $argv; or contains -- -h $argv
        echo "Usage: nh upgrade"
        echo ""
        echo "Upgrades packages across your system:"
        echo "  - Homebrew formulae and casks"
        echo "  - Mac App Store apps (via mas)"
        echo "  - ASDF: sets global versions to latest installed"
        echo ""
        echo "Emacs packages must be upgraded manually."
        return 0
    end

    echo "=== Homebrew ==="
    if command -q brew
        brew update
        brew upgrade
        _nh_set_last_checked homebrew
    else
        echo "  brew not installed, skipping"
    end

    echo ""
    echo "=== App Store ==="
    if command -q mas
        mas upgrade
        _nh_set_last_checked appstore
    else
        echo "  mas not installed, skipping"
    end

    echo ""
    echo "=== ASDF Globals ==="
    if command -q asdf
        for plugin in (asdf plugin list)
            set -l latest (asdf list $plugin 2>/dev/null | tail -1 | string trim | string replace '*' '')
            if test -n "$latest"
                echo "  Setting $plugin → $latest"
                asdf set --home $plugin $latest
            end
        end
        _nh_set_last_checked asdf
    else
        echo "  asdf not installed, skipping"
    end

    echo ""
    echo "=== Emacs ==="
    echo "  Upgrade manually: M-x package-upgrade-all"

    echo ""
    echo "Done!"
end
