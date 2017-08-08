#!/bin/sh
#/ This script draws heavily from Mike McQuaid's Strap project
#/
#/ Bootstrap system with dependencies

set -e

abort() { STEP="";   echo "!!! $*" >&2; exit 1; }
log()   { STEP="$*"; echo "--> $*"; }
logn()  { STEP="$*"; printf -- "--> %s " "$*"; }
logk()  { STEP="";   echo "OK"; echo }

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
log "Installing the Xcode Command Line Tools:"
DEVELOPER_DIR=$("xcode-select" -print-path 2>/dev/null || true)
if [ -z "$DEVELOPER_DIR" ] || ! [ -f "$DEVELOPER_DIR/usr/bin/git" ] \
                           || ! [ -f "/usr/include/iconv.h" ]
then
  CLT_PLACEHOLDER="/tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress"
  sudo touch "$CLT_PLACEHOLDER"
  CLT_PACKAGE=$(softwareupdate -l | \
                grep -B 1 -E "Command Line (Developer|Tools)" | \
                awk -F"*" '/^ +\*/ {print $2}' | sed 's/^ *//' | head -n1)
  sudo softwareupdate -i "$CLT_PACKAGE"
  sudo rm -f "$CLT_PLACEHOLDER"
  if ! [ -f "/usr/include/iconv.h" ]; then
    logn "Requesting user install of Xcode Command Line Tools:"
    xcode-select --install
  fi
  logk
fi

# Install Homebrew
# log "Installing Homebrew:"



# logk

# # Install any remaining software updates.
# logn "Installing software updates:"
# UPTODATE=$(softwareupdate -l 2>&1 | grep $Q "No new software available.")
# if [ -z $UPTODATE ]; then
#   sudo softwareupdate --install --all
#   xcode_license
# fi
# logk

# DONE! No More Bash!
SUCCESS="1"
log "System Bootstrapped!"
