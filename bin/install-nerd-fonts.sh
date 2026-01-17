#!/bin/bash
#
# Install all Nerd Fonts via Homebrew
#
# Usage:
#   ./bin/install-nerd-fonts.sh

set -e

echo ""
echo "========================================"
echo "   Installing Nerd Fonts"
echo "========================================"
echo ""

# Ensure Homebrew is in PATH
if command -v brew &>/dev/null; then
  eval "$(brew shellenv)"
elif [ -x /opt/homebrew/bin/brew ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
elif [ -x /usr/local/bin/brew ]; then
  eval "$(/usr/local/bin/brew shellenv)"
else
  echo "!!! Homebrew not found" >&2
  exit 1
fi

echo "--> Searching for Nerd Fonts..."
fonts=$(brew search '/font-.*-(nerd-font|nf)/' | awk '{ print $1 }')
font_count=$(echo "$fonts" | wc -l | tr -d ' ')

echo "    Found $font_count fonts"
echo "--> Installing (this may take a while)..."

# Install each font individually, continue on failure
for font in $fonts; do
  brew install --cask "$font" || echo "    Warning: $font failed, continuing..."
done

echo ""
echo "========================================"
echo "   Nerd Fonts Installed!"
echo "========================================"
echo ""
