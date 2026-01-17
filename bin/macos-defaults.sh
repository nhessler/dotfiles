#!/bin/bash
#
# macOS system preferences/defaults
# Sets various macOS settings to preferred values.
#
# Usage:
#   ./bin/macos-defaults.sh
#
# Some changes may require a logout or restart to take effect.

set -e

echo ""
echo "========================================"
echo "   macOS Defaults Configuration"
echo "========================================"
echo ""

# Close System Preferences to prevent it from overriding settings
osascript -e 'tell application "System Preferences" to quit' 2>/dev/null || true
osascript -e 'tell application "System Settings" to quit' 2>/dev/null || true

###############################################################################
# Keyboard & Input                                                            #
###############################################################################
echo "--> Configuring Keyboard & Input..."

# Fast key repeat (lower = faster)
defaults write NSGlobalDomain KeyRepeat -int 2
defaults write NSGlobalDomain InitialKeyRepeat -int 15

# Disable auto-capitalization
defaults write NSGlobalDomain NSAutomaticCapitalizationEnabled -bool false

# Disable smart dashes
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false

# Disable smart quotes
defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false

# Full keyboard access (Tab through all UI controls)
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

###############################################################################
# Trackpad                                                                    #
###############################################################################
echo "--> Configuring Trackpad..."

# Tap to click
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults write com.apple.AppleMultitouchTrackpad Clicking -bool true
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1

###############################################################################
# Finder                                                                      #
###############################################################################
echo "--> Configuring Finder..."

# Show file extensions
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

# Show hidden files
defaults write com.apple.finder AppleShowAllFiles -bool true

# Show path bar
defaults write com.apple.finder ShowPathbar -bool true

# Show status bar
defaults write com.apple.finder ShowStatusBar -bool true

# Show POSIX path in title
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

# Folders on top when sorting by name
defaults write com.apple.finder _FXSortFoldersFirst -bool true

# Search current folder by default
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

# Disable .DS_Store on network volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

# Disable .DS_Store on USB volumes
defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true

# Column view by default
# Flwv = Cover Flow, Nlsv = List, clmv = Column, icnv = Icon
defaults write com.apple.finder FXPreferredViewStyle -string "clmv"

# Show ~/Library folder
chflags nohidden ~/Library

# Show /Volumes folder
sudo chflags nohidden /Volumes

# Allow quitting Finder with Cmd+Q
defaults write com.apple.finder QuitMenuItem -bool true

###############################################################################
# Dock                                                                        #
###############################################################################
echo "--> Configuring Dock..."

# Auto-hide Dock
defaults write com.apple.dock autohide -bool true

# Remove auto-hide delay
defaults write com.apple.dock autohide-delay -float 0

# Remove auto-hide animation
defaults write com.apple.dock autohide-time-modifier -float 0

# Icon size 36 pixels
defaults write com.apple.dock tilesize -int 36

# Minimize windows to app icon
defaults write com.apple.dock minimize-to-application -bool true

# Disable animations (scale effect instead of genie)
defaults write com.apple.dock mineffect -string "scale"

# Translucent icons for hidden apps
defaults write com.apple.dock showhidden -bool true

# Don't show recent apps
defaults write com.apple.dock show-recents -bool false

# Position Dock on left
defaults write com.apple.dock orientation -string "left"

###############################################################################
# Screenshots                                                                 #
###############################################################################
echo "--> Configuring Screenshots..."

# Create screenshots folder
mkdir -p ~/Documents/Screenshots

# Save screenshots to ~/Documents/Screenshots
defaults write com.apple.screencapture location -string "$HOME/Documents/Screenshots"

# Save screenshots as PNG
defaults write com.apple.screencapture type -string "png"

# Disable shadow in screenshots
defaults write com.apple.screencapture disable-shadow -bool true

###############################################################################
# Security                                                                    #
###############################################################################
echo "--> Configuring Security..."

# Require password immediately after sleep or screen saver
defaults write com.apple.screensaver askForPassword -int 1
defaults write com.apple.screensaver askForPasswordDelay -int 0

# Enable firewall
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setglobalstate on

###############################################################################
# Energy & Sleep                                                              #
###############################################################################
echo "--> Configuring Energy & Sleep..."

# Display sleep: 15 minutes
sudo pmset -a displaysleep 15

# Disable machine sleep while charging
sudo pmset -c sleep 0

# Enable lid wakeup
sudo pmset -a lidwake 1

###############################################################################
# Safari                                                                      #
###############################################################################
echo "--> Configuring Safari..."

# Show full URL in address bar
defaults write com.apple.Safari ShowFullURLInSmartSearchField -bool true

# Enable Develop menu
defaults write com.apple.Safari IncludeDevelopMenu -bool true
defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey -bool true
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled -bool true

# Don't open "safe" files after downloading
defaults write com.apple.Safari AutoOpenSafeDownloads -bool false

###############################################################################
# Text Editing                                                                #
###############################################################################
echo "--> Configuring Text Editing..."

# TextEdit: use plain text by default
defaults write com.apple.TextEdit RichText -int 0

# TextEdit: open and save as UTF-8
defaults write com.apple.TextEdit PlainTextEncoding -int 4
defaults write com.apple.TextEdit PlainTextEncodingForWrite -int 4

# Enable spell check while typing
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false
defaults write NSGlobalDomain WebContinuousSpellCheckingEnabled -bool true

###############################################################################
# Misc                                                                        #
###############################################################################
echo "--> Configuring Misc..."

# Expand save panel by default
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true

# Expand print panel by default
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true

# Check for software updates daily
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1

# Disable Spotlight indexing for external volumes
sudo mdutil -i off /Volumes 2>/dev/null || true

###############################################################################
# Kill affected applications                                                  #
###############################################################################
echo ""
echo "--> Restarting affected applications..."

for app in "Dock" "Finder" "Safari" "SystemUIServer"; do
  killall "$app" &>/dev/null || true
done

echo ""
echo "========================================"
echo "   Done!"
echo "========================================"
echo ""
echo "Some changes require a logout or restart to take effect."
echo ""
