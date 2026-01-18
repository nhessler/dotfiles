#!/usr/bin/env fish

function nh -d "Nathan Hessler's personal CLI helper"
    set -l DOTFILES_DIR "$HOME/Projects/nhessler/dotfiles"

    # No arguments - show help
    if test (count $argv) -eq 0
        _nh_help
        return 0
    end

    # Dispatch subcommands
    set -l cmd $argv[1]
    set -l args $argv[2..-1]

    switch $cmd
        case setup
            _nh_setup $args
        case help -h --help
            _nh_help
        case '*'
            echo "nh: unknown command '$cmd'"
            echo ""
            _nh_help
            return 1
    end
end

function _nh_help
    echo "Usage: nh <command> [options]"
    echo ""
    echo "Nathan Hessler's personal CLI helper."
    echo ""
    echo "Commands:"
    echo "  setup       Run the dotfiles setup script"
    echo "  help        Show this help message"
    echo ""
    echo "Run 'nh <command> --help' for command-specific help."
end

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
