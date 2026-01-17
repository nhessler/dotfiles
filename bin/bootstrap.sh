#!/bin/bash
#
# Bootstrap script for macOS dotfiles
# Inspired by Mike McQuaid's Strap project
# https://github.com/nhessler/dotfiles
#
# Usage:
#   curl -fsSL https://raw.githubusercontent.com/nhessler/dotfiles/main/bin/bootstrap.sh | bash
#   or
#   ./bin/bootstrap.sh
#
# After bootstrap completes, run bin/install.sh to install packages and languages.

set -e

#
# Configuration
#
DOTFILES_REPO="git@github.com:nhessler/dotfiles.git"
DOTFILES_DIR="${DOTFILES_DIR:-$HOME/Projects/nhessler/dotfiles}"

# Homebrew paths by architecture
HOMEBREW_PREFIX_ARM64="/opt/homebrew"
HOMEBREW_PREFIX_INTEL="/usr/local"

# Minimum macOS version for TouchID sudo_local (Sonoma = 14.0)
TOUCHID_SUDO_LOCAL_MIN="14"

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

logn() {
  STEP="$*"
  printf -- "--> %s " "$*"
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
      echo "!!! BOOTSTRAP FAILED" >&2
    fi
  fi
}

trap cleanup EXIT

#
# Utility Functions
#
require_sudo() {
  log "Requesting sudo access:"
  if ! sudo -v; then
    abort "Failed to obtain sudo access"
  fi
  # Keep sudo timestamp fresh in background
  while true; do
    sudo -n true
    sleep 60
    kill -0 "$$" 2>/dev/null || exit
  done &
  logk
}

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

macos_major_version() {
  sw_vers -productVersion | cut -d. -f1
}

command_exists() {
  command -v "$1" >/dev/null 2>&1
}

#
# Dotfiles Functions
#
check_github_ssh() {
  log "Checking GitHub SSH access:"

  if ssh -T git@github.com 2>&1 | grep -q "successfully authenticated"; then
    echo "    SSH authentication to GitHub working"
    logk
    return
  fi

  echo ""
  echo "!!! GitHub SSH authentication failed"
  echo ""
  echo "Please set up SSH for GitHub before continuing:"
  echo ""
  echo "  1. Create an SSH key:"
  echo "     https://github.com/nhessler/dotfiles/blob/main/docs/ssh-setup.md"
  echo "     https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent"
  echo ""
  echo "  2. Configure SSH and add key to GitHub:"
  echo "     https://github.com/nhessler/dotfiles/blob/main/docs/ssh-setup.md"
  echo ""
  echo "  3. Re-run this script"
  echo ""
  exit 1
}

clone_dotfiles() {
  log "Checking dotfiles repository:"

  if [ -d "$DOTFILES_DIR/.git" ]; then
    echo "    Already cloned at $DOTFILES_DIR"
    echo "    Pulling latest changes..."
    (cd "$DOTFILES_DIR" && git pull)
    logk
    return
  fi

  # Create parent directory if needed
  local parent_dir
  parent_dir=$(dirname "$DOTFILES_DIR")
  if [ ! -d "$parent_dir" ]; then
    echo "    Creating $parent_dir"
    mkdir -p "$parent_dir"
  fi

  echo "    Cloning $DOTFILES_REPO to $DOTFILES_DIR..."
  git clone "$DOTFILES_REPO" "$DOTFILES_DIR"

  logk
}

#
# Security Functions
#
setup_filevault() {
  log "Checking FileVault (Full Disk Encryption):"

  if fdesetup status | grep -q "FileVault is On"; then
    echo "    FileVault is already enabled"
    logk
    return
  fi

  echo "    FileVault is not enabled. Enabling now..."
  echo "    (Recovery key will be saved to your Desktop)"
  echo ""

  # Enable FileVault - this is interactive and will prompt for password
  if sudo fdesetup enable -user "$(whoami)" 2>/dev/null | tee "$HOME/Desktop/FileVault_Recovery_Key.txt"; then
    echo ""
    echo "    Recovery key saved to: $HOME/Desktop/FileVault_Recovery_Key.txt"
    echo "    IMPORTANT: Store this key somewhere safe and delete the file!"
  else
    warn "FileVault enable failed or was cancelled. Enable manually via System Settings."
  fi
  logk
}

setup_firewall() {
  log "Checking Application Layer Firewall:"

  local fw_state
  fw_state=$(/usr/libexec/ApplicationFirewall/socketfilterfw --getglobalstate 2>/dev/null || echo "")

  if echo "$fw_state" | grep -q "enabled"; then
    echo "    Firewall is already enabled"
  else
    echo "    Enabling firewall..."
    sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setglobalstate on >/dev/null
  fi
  logk
}

setup_touchid_sudo() {
  log "Checking TouchID for sudo:"

  local macos_ver
  macos_ver=$(macos_major_version)

  # Check macOS version (need Sonoma 14+ for sudo_local)
  if [ "$macos_ver" -lt "$TOUCHID_SUDO_LOCAL_MIN" ]; then
    echo "    Skipping: macOS $macos_ver doesn't support sudo_local (need $TOUCHID_SUDO_LOCAL_MIN+)"
    logk
    return
  fi

  # Check if TouchID hardware is available
  if ! /usr/bin/bioutil -r 2>/dev/null | grep -qi "touch id"; then
    echo "    Skipping: TouchID hardware not detected"
    logk
    return
  fi

  local sudo_local="/etc/pam.d/sudo_local"
  local sudo_local_template="/etc/pam.d/sudo_local.template"

  # Check if already configured
  if [ -f "$sudo_local" ] && grep -q "pam_tid.so" "$sudo_local" && ! grep -q "^#.*pam_tid.so" "$sudo_local"; then
    echo "    TouchID for sudo is already configured"
    logk
    return
  fi

  echo "    Configuring TouchID for sudo..."

  # Create sudo_local from template if it doesn't exist
  if [ ! -f "$sudo_local" ] && [ -f "$sudo_local_template" ]; then
    sudo cp "$sudo_local_template" "$sudo_local"
  fi

  # If sudo_local exists, uncomment the pam_tid line or add it
  if [ -f "$sudo_local" ]; then
    if grep -q "#.*pam_tid.so" "$sudo_local"; then
      # Uncomment existing line
      sudo sed -i '' 's/^#.*\(auth.*sufficient.*pam_tid.so\)/\1/' "$sudo_local"
    elif ! grep -q "pam_tid.so" "$sudo_local"; then
      # Add the line after the first comment block
      echo "auth       sufficient     pam_tid.so" | sudo tee -a "$sudo_local" >/dev/null
    fi
  else
    # Create new sudo_local with just the tid line
    echo "auth       sufficient     pam_tid.so" | sudo tee "$sudo_local" >/dev/null
  fi

  echo "    TouchID for sudo configured"
  logk
}

#
# Installation Functions
#
install_xcode_cli_tools() {
  log "Checking Xcode Command Line Tools:"

  if xcode-select -p &>/dev/null; then
    echo "    Already installed"
    logk
    return
  fi

  echo "    Installing Xcode Command Line Tools..."

  # Trigger installation via softwareupdate
  local placeholder="/tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress"
  sudo touch "$placeholder"

  local clt_package
  clt_package=$(softwareupdate -l 2>/dev/null | \
                grep -E "Label:.*Command Line" | \
                sed 's/^.*Label: //' | \
                sort -V | tail -1)

  if [ -n "$clt_package" ]; then
    sudo softwareupdate -i "$clt_package"
  else
    # Fallback to interactive install
    sudo rm -f "$placeholder"
    xcode-select --install
    echo "    Please complete the installation dialog, then re-run this script."
    exit 0
  fi

  sudo rm -f "$placeholder"

  # Accept Xcode license if needed
  if /usr/bin/xcrun clang 2>&1 | grep -q "license"; then
    echo "    Accepting Xcode license..."
    sudo xcodebuild -license accept
  fi

  logk
}

install_rosetta() {
  # Only needed on Apple Silicon
  if ! is_arm64; then
    return
  fi

  log "Checking Rosetta:"

  if /usr/bin/pgrep -q oahd; then
    echo "    Already installed"
    logk
    return
  fi

  echo "    Installing Rosetta..."
  softwareupdate --install-rosetta --agree-to-license

  logk
}

install_homebrew() {
  log "Checking Homebrew:"

  local brew_prefix
  brew_prefix=$(homebrew_prefix)

  if [ -x "$brew_prefix/bin/brew" ]; then
    echo "    Already installed at $brew_prefix"
    echo "    Updating Homebrew..."
    "$brew_prefix/bin/brew" update
    logk
    return
  fi

  echo "    Installing Homebrew at $brew_prefix..."

  # Create directory structure
  sudo mkdir -p "$brew_prefix"
  sudo chown -R "$(whoami):admin" "$brew_prefix"

  # Clone Homebrew via git
  git clone --depth 1 https://github.com/Homebrew/brew.git "$brew_prefix/Homebrew"

  # Create required directories and symlinks
  mkdir -p "$brew_prefix/bin"
  ln -sf "$brew_prefix/Homebrew/bin/brew" "$brew_prefix/bin/brew"

  # Add to current session PATH
  eval "$("$brew_prefix/bin/brew" shellenv)"

  # Run initial update
  "$brew_prefix/bin/brew" update --force

  logk
}

install_asdf() {
  log "Checking ASDF Version Manager:"

  local brew_prefix
  brew_prefix=$(homebrew_prefix)

  if [ ! -x "$brew_prefix/bin/brew" ]; then
    abort "Homebrew is required to install ASDF"
  fi

  # Ensure Homebrew is in PATH
  eval "$("$brew_prefix/bin/brew" shellenv)"

  if brew list asdf &>/dev/null; then
    echo "    Already installed via Homebrew"
    logk
    return
  fi

  echo "    Installing ASDF via Homebrew..."
  brew install asdf
  logk
}

#
# Configuration Functions
#
setup_directories() {
  log "Setting up directories:"

  local dirs="$HOME/Projects"

  for dir in $dirs; do
    if [ ! -d "$dir" ]; then
      echo "    Creating $dir"
      mkdir -p "$dir"
    else
      echo "    $dir exists"
    fi
  done

  logk
}

create_symlinks() {
  log "Creating dotfile symlinks:"

  # Symlink pairs: source target
  local pairs="
    dot-config:$HOME/.config
    dot-hammerspoon:$HOME/.hammerspoon
    dot-claude:$HOME/.claude
  "

  for pair in $pairs; do
    local source="${pair%%:*}"
    local target="${pair##*:}"
    local src_path="$DOTFILES_DIR/$source"

    if [ ! -e "$src_path" ]; then
      warn "Source $src_path does not exist, skipping"
      continue
    fi

    if [ -L "$target" ]; then
      local current
      current=$(readlink "$target")
      if [ "$current" = "$src_path" ]; then
        echo "    $target -> $src_path (already linked)"
        continue
      else
        echo "    Removing old symlink: $target -> $current"
        rm "$target"
      fi
    elif [ -e "$target" ]; then
      echo "    Backing up: $target -> ${target}.backup"
      mv "$target" "${target}.backup"
    fi

    echo "    Linking: $target -> $src_path"
    ln -s "$src_path" "$target"
  done

  logk
}

install_software_updates() {
  log "Checking for software updates:"

  local updates
  updates=$(softwareupdate -l 2>&1)

  if echo "$updates" | grep -q "No new software available"; then
    echo "    Software is up to date"
  else
    echo "    Installing available updates..."
    sudo softwareupdate --install --all
  fi
  logk
}

#
# Main
#
main() {
  echo ""
  echo "========================================"
  echo "   macOS Dotfiles Bootstrap Script"
  echo "========================================"
  echo ""

  # Verify we're on macOS
  if [ "$(uname)" != "Darwin" ]; then
    abort "This script is for macOS only"
  fi

  # Get sudo access
  require_sudo

  # Phase 1: System Prerequisites (need git for cloning)
  install_xcode_cli_tools
  install_rosetta

  # Phase 2: Get Dotfiles
  check_github_ssh
  clone_dotfiles

  # Verify dotfiles are in place
  if [ ! -d "$DOTFILES_DIR/dot-config" ]; then
    abort "Dotfiles not found at $DOTFILES_DIR after clone attempt"
  fi

  # Phase 3: Security Hardening
  setup_filevault
  setup_firewall
  setup_touchid_sudo

  # Phase 4: Package Management
  install_homebrew
  install_asdf

  # Phase 5: Dotfiles Setup
  setup_directories
  create_symlinks

  # Phase 6: Software Updates (last - may require restart)
  install_software_updates

  # Success!
  SUCCESS="1"

  echo ""
  echo "========================================"
  echo "   Bootstrap Complete!"
  echo "========================================"
  echo ""
  echo "Next steps:"
  echo "  1. Restart your machine:"
  echo "     a. If updates are pending: System Settings → General → Software Update → Restart"
  echo "     b. Otherwise: restart normally"
  echo "  2. Run the install script:"
  echo "       $DOTFILES_DIR/bin/install.sh"
  echo ""
}

# Run
main "$@"
