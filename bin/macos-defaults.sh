#!/bin/bash
#
# macOS system preferences/defaults
# Sets various macOS settings to preferred values.
#
# Usage:
#   ./bin/macos-defaults.sh
#
# Review and customize these settings before running.
# Some changes may require a logout or restart to take effect.

set -e

echo ""
echo "========================================"
echo "   macOS Defaults Configuration"
echo "========================================"
echo ""

# TODO: Add your preferred macOS defaults here
# Examples:
#
# # Dock
# defaults write com.apple.dock autohide -bool true
# defaults write com.apple.dock tilesize -int 48
#
# # Finder
# defaults write com.apple.finder ShowPathbar -bool true
# defaults write com.apple.finder ShowStatusBar -bool true
#
# # Keyboard
# defaults write NSGlobalDomain KeyRepeat -int 2
# defaults write NSGlobalDomain InitialKeyRepeat -int 15
#
# # Screenshots
# defaults write com.apple.screencapture location -string "$HOME/Desktop"
# defaults write com.apple.screencapture type -string "png"

echo "No defaults configured yet."
echo "Edit bin/macos-defaults.sh to add your preferences."
echo ""
