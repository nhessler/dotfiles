#!/bin/bash
#
# Install script for macOS dotfiles
# Installs Brewfile packages, ASDF languages, and sets default shell.
#
# Usage:
#   ./bin/install.sh [--skip-mas]
#
# Options:
#   --skip-mas    Skip Mac App Store apps (use if not signed in)
#
# Run this after bootstrap.sh completes.

set -e

#
# Configuration
#
DOTFILES_DIR="${DOTFILES_DIR:-$HOME/Projects/nhessler/dotfiles}"

# Parse arguments
for arg in "$@"; do
  case $arg in
    --skip-mas)
      export DOTFILES_INSTALL_SKIP_MAS=1
      ;;
  esac
done

# Homebrew paths by architecture
HOMEBREW_PREFIX_ARM64="/opt/homebrew"
HOMEBREW_PREFIX_INTEL="/usr/local"

# ASDF plugins to install (order matters: erlang before elixir)
ASDF_PLUGINS="ruby erlang elixir nodejs"

# State tracking
STEP=""
SUCCESS=""

#
# Logging Functions
#
log() {
  STEP="$*"
  echo "--> $*"
}

logk() {
  STEP=""
  echo "OK"
}

abort() {
  STEP=""
  echo "!!! $*" >&2
  exit 1
}

warn() {
  echo "WARNING: $*" >&2
}

cleanup() {
  set +e
  if [ -z "$SUCCESS" ]; then
    if [ -n "$STEP" ]; then
      echo "!!! $STEP FAILED" >&2
    else
      echo "!!! INSTALL FAILED" >&2
    fi
  fi
}

trap cleanup EXIT

#
# Utility Functions
#
is_arm64() {
  [ "$(uname -m)" = "arm64" ]
}

homebrew_prefix() {
  if is_arm64; then
    echo "$HOMEBREW_PREFIX_ARM64"
  else
    echo "$HOMEBREW_PREFIX_INTEL"
  fi
}

#
# Installation Functions
#
install_brewfile() {
  log "Installing Brewfile packages:"

  local brew_prefix
  brew_prefix=$(homebrew_prefix)

  if [ ! -x "$brew_prefix/bin/brew" ]; then
    abort "Homebrew not installed. Run bootstrap.sh first."
  fi

  # Ensure Homebrew is in PATH
  eval "$("$brew_prefix/bin/brew" shellenv)"

  # Set XDG_CONFIG_HOME for brew bundle --global
  export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"

  # Skip Mac App Store apps if requested (set by --skip-mas flag)
  if [ -n "$DOTFILES_INSTALL_SKIP_MAS" ]; then
    echo "    Skipping Mac App Store apps (--skip-mas)"
    local brewfile="$XDG_CONFIG_HOME/homebrew/Brewfile"
    local mas_apps
    mas_apps=$(grep "^  mas\|^mas" "$brewfile" | sed 's/.*mas "\([^"]*\)".*/\1/' | tr '\n' ' ')
    export HOMEBREW_BUNDLE_MAS_SKIP="$mas_apps"
  fi

  # Uses XDG location: ~/.config/homebrew/Brewfile
  echo "    Running brew bundle (this may take a while)..."
  brew bundle upgrade --global

  logk
}

install_asdf_plugins() {
  log "Installing ASDF plugins and languages:"

  local brew_prefix
  brew_prefix=$(homebrew_prefix)

  # Ensure Homebrew is in PATH
  eval "$("$brew_prefix/bin/brew" shellenv)"

  # Source ASDF for this session
  local asdf_sh
  if [ -f "$brew_prefix/opt/asdf/libexec/asdf.sh" ]; then
    asdf_sh="$brew_prefix/opt/asdf/libexec/asdf.sh"
  else
    abort "ASDF not found. Run bootstrap.sh first."
  fi

  # shellcheck source=/dev/null
  . "$asdf_sh"

  for plugin in $ASDF_PLUGINS; do
    echo "    Setting up $plugin..."

    # Add plugin if not already added
    if ! asdf plugin list 2>/dev/null | grep -q "^$plugin$"; then
      echo "      Adding plugin..."
      asdf plugin add "$plugin"
    fi

    # Install latest version
    echo "      Installing latest version..."
    local latest
    latest=$(asdf latest "$plugin")
    if [ -n "$latest" ]; then
      if ! asdf list "$plugin" 2>/dev/null | grep -q "$latest"; then
        asdf install "$plugin" "$latest"
      fi
      # Set as home default
      asdf set --home "$plugin" "$latest"
      echo "      Set $plugin $latest as home default"
    else
      warn "Could not determine latest version for $plugin"
    fi
  done

  logk
}

set_default_shell() {
  log "Setting default shell to Fish:"

  local fish_path
  fish_path=$(which fish 2>/dev/null || echo "")

  if [ -z "$fish_path" ]; then
    # Try common Homebrew locations
    local brew_prefix
    brew_prefix=$(homebrew_prefix)
    if [ -x "$brew_prefix/bin/fish" ]; then
      fish_path="$brew_prefix/bin/fish"
    else
      warn "Fish not found, skipping shell change"
      logk
      return
    fi
  fi

  # Check if Fish is already the default shell
  if [ "$SHELL" = "$fish_path" ]; then
    echo "    Fish is already the default shell"
    logk
    return
  fi

  # Add Fish to /etc/shells if not present
  if ! grep -q "^$fish_path$" /etc/shells; then
    echo "    Adding $fish_path to /etc/shells"
    echo "$fish_path" | sudo tee -a /etc/shells >/dev/null
  fi

  # Change default shell
  echo "    Changing default shell to $fish_path"
  chsh -s "$fish_path"

  logk
}

#
# Main
#
main() {
  echo ""
  echo "========================================"
  echo "   macOS Dotfiles Install Script"
  echo "========================================"
  echo ""

  # Verify we're on macOS
  if [ "$(uname)" != "Darwin" ]; then
    abort "This script is for macOS only"
  fi

  # Verify dotfiles exist
  if [ ! -d "$DOTFILES_DIR/dot-config" ]; then
    abort "Dotfiles not found at $DOTFILES_DIR. Run bootstrap.sh first."
  fi

  # Phase 1: Brewfile packages (includes build deps for languages)
  install_brewfile

  # Phase 2: ASDF plugins and languages
  install_asdf_plugins

  # Phase 3: Set default shell (Fish now installed via Brewfile)
  set_default_shell

  # Success!
  SUCCESS="1"

  echo ""
  echo "========================================"
  echo "   Install Complete!"
  echo "========================================"
  echo ""
  echo "Next steps:"
  echo "  1. Restart your terminal (or run: exec fish)"
  echo "  2. Optionally run bin/macos-defaults.sh for system preferences"
  echo ""
}

# Run
main "$@"
