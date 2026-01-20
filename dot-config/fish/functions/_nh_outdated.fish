# nh outdated subcommand

function _nh_outdated
    # Handle help flag
    if contains -- --help $argv; or contains -- -h $argv
        echo "Usage: nh outdated [mark <category>|mark all]"
        echo ""
        echo "Check for outdated dependencies across your system."
        echo ""
        echo "Subcommands:"
        echo "  (none)        Show outdated status for all categories"
        echo "  mark <cat>    Mark a category as checked (updates timestamp)"
        echo "  mark all      Mark all categories as checked"
        echo ""
        echo "Categories: homebrew, asdf, appstore, macos, emacs"
        echo ""
        echo "Timestamps are stored in ~/.local/state/nh/outdated-checks"
        return 0
    end

    # Handle mark subcommand
    if test (count $argv) -ge 1; and test "$argv[1]" = "mark"
        _nh_outdated_mark $argv[2..-1]
        return $status
    end

    # Ensure state directory exists
    _nh_ensure_state_dir

    # Run all checks
    echo ""
    _nh_outdated_homebrew
    echo ""
    _nh_outdated_asdf
    echo ""
    _nh_outdated_macos
    echo ""
    _nh_outdated_appstore
    echo ""
    _nh_outdated_emacs
    echo ""
end

function _nh_outdated_mark
    if test (count $argv) -eq 0
        echo "Usage: nh outdated mark <category|all>"
        echo "Categories: homebrew, asdf, appstore, macos, emacs, all"
        return 1
    end

    set -l category $argv[1]
    set -l valid_categories homebrew asdf appstore macos emacs

    if test "$category" = "all"
        for cat in $valid_categories
            _nh_set_last_checked $cat
            echo "Marked $cat as checked."
        end
        return 0
    end

    if not contains $category $valid_categories
        echo "Unknown category: $category"
        echo "Valid categories: $valid_categories all"
        return 1
    end

    _nh_set_last_checked $category
    echo "Marked $category as checked."
end

#
# Individual checkers
#

function _nh_outdated_homebrew
    set -l last_checked (_nh_get_last_checked homebrew)
    echo "Homebrew ($last_checked):"

    if not command -q brew
        echo "  brew not installed"
        return 1
    end

    set -l outdated (brew outdated --quiet)
    set -l count (count $outdated)

    if test $count -eq 0
        echo "  Everything up to date"
    else
        echo "  $count packages outdated:"
        for pkg in $outdated[1..10]  # Show first 10
            echo "    $pkg"
        end
        if test $count -gt 10
            echo "    ... and "(math $count - 10)" more"
        end
        echo ""
        echo "  Run: brew upgrade"
    end
end

function _nh_outdated_asdf
    set -l last_checked (_nh_get_last_checked asdf)
    echo "ASDF ($last_checked):"

    if not command -q asdf
        echo "  asdf not installed"
        return 1
    end

    # Update plugins first to get latest version info
    echo "  Updating plugins..."
    asdf plugin update --all >/dev/null 2>&1

    set -l plugins (asdf plugin list 2>/dev/null)
    if test (count $plugins) -eq 0
        echo "  No plugins installed"
        return 0
    end

    set -l has_outdated 0

    for plugin in $plugins
        set -l latest (asdf latest $plugin 2>/dev/null)
        if test -z "$latest"
            continue
        end

        # Get installed versions, stripping the * prefix from current version
        set -l installed (asdf list $plugin 2>/dev/null | string trim | string replace -r '^\*' '')

        if contains $latest $installed
            echo "  $plugin: up to date ($latest)"
        else
            set has_outdated 1
            set -l current_versions (string join ", " $installed)
            echo "  $plugin: $latest available (installed: $current_versions)"
        end
    end

    if test $has_outdated -eq 1
        echo ""
        echo "  Run: asdf install <plugin> <version>"
    end
end

function _nh_outdated_macos
    set -l last_checked (_nh_get_last_checked macos)
    echo "macOS ($last_checked):"

    # softwareupdate -l can be slow, so just remind
    echo "  Run 'softwareupdate -l' to check for updates"
    echo "  Or check System Settings → General → Software Update"
end

function _nh_outdated_appstore
    set -l last_checked (_nh_get_last_checked appstore)
    echo "App Store ($last_checked):"

    if command -q mas
        set -l outdated (mas outdated 2>/dev/null)
        set -l count (count $outdated)

        if test $count -eq 0
            echo "  Everything up to date"
        else
            echo "  $count apps outdated:"
            for app in $outdated[1..5]
                echo "    $app"
            end
            if test $count -gt 5
                echo "    ... and "(math $count - 5)" more"
            end
            echo ""
            echo "  Run: mas upgrade"
        end
    else
        echo "  Check manually: open -a 'App Store'"
        echo "  (Install 'mas' for CLI access: brew install mas)"
    end
end

function _nh_outdated_emacs
    set -l last_checked (_nh_get_last_checked emacs)
    echo "Emacs packages ($last_checked):"
    echo "  Check manually in Emacs:"
    echo "    M-x list-packages, then U to mark upgrades"
    echo "    Or M-x package-refresh-contents, then M-x package-upgrade-all"
end
