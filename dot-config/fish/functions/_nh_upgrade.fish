function _nh_upgrade -d "Upgrade system packages and set asdf globals to latest installed"
    if contains -- --help $argv; or contains -- -h $argv
        echo "Usage: nh upgrade"
        echo ""
        echo "Upgrades packages across your system:"
        echo "  - Homebrew formulae and casks"
        echo "  - Mac App Store apps (via mas)"
        echo "  - ASDF: sets global versions to latest installed"
        echo "  - Emacs packages (via batch mode)"
        echo "  - macOS: reminder only (manual due to potential restarts)"
        return 0
    end

    echo "=== Homebrew ==="
    if command -q brew
        brew update
        brew upgrade
        _nh_set_last_upgraded homebrew
    else
        echo "  brew not installed, skipping"
    end

    echo ""
    echo "=== App Store ==="
    if command -q mas
        mas upgrade
        _nh_set_last_upgraded appstore
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
        _nh_set_last_upgraded asdf
    else
        echo "  asdf not installed, skipping"
    end

    echo ""
    echo "=== Emacs ==="
    if command -q emacs
        echo "  Refreshing package index..."
        emacs --batch \
            --eval '(package-refresh-contents)' \
            --eval '(package-upgrade-all)' \
            2>&1 | grep -v "^Contacting\|^Package\|^Done\|^Importing\|^\$"
        _nh_set_last_upgraded emacs
    else
        echo "  emacs not installed, skipping"
    end

    echo ""
    echo "=== macOS ==="
    echo "  Apply manually via System Settings → General → Software Update"
    echo "  (Skipped here because updates may require restarts)"
    echo "  Mark as upgraded after applying: nh outdated mark macos"

    echo ""
    echo "Done!"
end
