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
        case outdated
            _nh_outdated $args
        case upgrade
            _nh_upgrade $args
        case add-keys
            _nh_add_keys $args
        case claude
            _nh_claude $args
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
    echo "  outdated    Check for outdated dependencies"
    echo "  upgrade     Upgrade brew, App Store, and asdf globals"
    echo "  add-keys    Add SSH keys to agent using keychain"
    echo "  claude      Open Claude Code in a named tab"
    echo "  help        Show this help message"
    echo ""
    echo "Run 'nh <command> --help' for command-specific help."
end
