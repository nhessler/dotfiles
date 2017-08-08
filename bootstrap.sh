#!/bin/bash
#/ This script draws heavily from Mike McQuaid's Strap project
#/
#/ Bootstrap system with dependencies

set -e

abort() { STEP="";   echo "!!! $*" >&2; exit 1; }
log()   { STEP="$*"; echo "--> $*"; }
logk()  { STEP="";   echo "OK"; echo; }

cleanup() {
  set +e

  if [ -z "$SUCCESS" ]; then
    if [ -n "$STEP" ]; then
      echo "!!! $STEP FAILED" >&2
    else
      echo "!!! UNKNOWN FAILURE" >&2
    fi
  fi
}

trap "cleanup" EXIT

# Install the Xcode Command Line Tools.
log "Checking Xcode Command Line Tools:"
DEVELOPER_DIR=$("xcode-select" -print-path 2>/dev/null || true)
if [ -z "$DEVELOPER_DIR" ] || ! [ -f "$DEVELOPER_DIR/usr/bin/git" ] \
                           || ! [ -f "/usr/include/iconv.h" ]
then
  log "Installing Xcode Command Line Tools"
  CLT_PLACEHOLDER="/tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress"
  sudo touch "$CLT_PLACEHOLDER"
  CLT_PACKAGE=$(softwareupdate -l | \
                grep -B 1 -E "Command Line (Developer|Tools)" | \
                awk -F"*" '/^ +\*/ {print $2}' | sed 's/^ *//' | head -n1)
  sudo softwareupdate -i "$CLT_PACKAGE"
  sudo rm -f "$CLT_PLACEHOLDER"
  if ! [ -f "/usr/include/iconv.h" ]; then
    xcode-select --install
  fi
else
  log "Xcode Command Line Tools up to date"
fi
logk

# Install any remaining software updates.
log "Checking remaining software updates:"
UPTODATE="$(softwareupdate -l 2>&1 | grep -q "No new software available.")"
if [ -z "$UPTODATE" ]; then
  log "Installing remaining software udpates:" 
  sudo softwareupdate --install --all
else
  log "Remaining software up to date"
fi
logk

# Agree to Xcode license.
log "Checking to Xcode license agreement:"
if /usr/bin/xcrun clang 2>&1 | grep -q "license"; then
  log "Agreeing to Xcode license agreement:"
  sudo xcodebuild -license
else
  log "Xcode license agreement accepted:"
fi
logk

# Install Homebrew.
# log "Installing Homebrew:"
# HOMEBREW_PRESENT=$(which brew)
# if [ -z $HOMEBREW_PRESENT ]; then
#   /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
# fi
# logk



# DONE! No More Bash!
SUCCESS="1"
log "System Bootstrapped!"