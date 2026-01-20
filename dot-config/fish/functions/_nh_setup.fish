# nh setup subcommand

function _nh_setup
    set -l DOTFILES_DIR "$HOME/Projects/nhessler/dotfiles"
    set -l setup_script "$DOTFILES_DIR/bin/setup.sh"

    # Handle help flag
    if contains -- --help $argv; or contains -- -h $argv
        echo "Usage: nh setup [--skip-mas]"
        echo ""
        echo "Run the dotfiles setup script."
        echo ""
        echo "Options:"
        echo "  --skip-mas    Skip Mac App Store apps (use if not signed in)"
        echo ""
        echo "This installs:"
        echo "  - Brewfile packages (Homebrew formulae, casks, fonts)"
        echo "  - ASDF plugins and latest language versions"
        echo "  - Sets Fish as default shell"
        echo "  - Installs Nerd Fonts"
        echo "  - Applies macOS defaults"
        return 0
    end

    # Verify setup script exists
    if not test -x "$setup_script"
        echo "ERROR: Setup script not found at $setup_script"
        return 1
    end

    # Run setup with any passed arguments
    bash "$setup_script" $argv
end
