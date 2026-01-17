# Post-Setup Checklist

Manual configuration steps after running `bootstrap.sh` and `setup.sh`.

## Prerequisites

- [ ] **Apple ID** - Sign in via System Settings → Apple ID
- [ ] **App Store** - Sign in to download mas apps (run `setup.sh` again if skipped)

## iCloud/Cloud Sync

These apps sync settings once connected to a sync folder:

- [ ] **1Password** - Sign in to sync vaults
- [ ] **Alfred** - Preferences → Advanced → Syncing → Set to Dropbox folder
- [ ] **Dash** - Preferences → Sync → Set sync folder
- [ ] **Obsidian** - Open vault from iCloud Drive
- [ ] **OmniFocus** - Sign in to sync

## License Keys

Check email or license manager for keys:

- [ ] **Alfred** - Powerpack license
- [ ] **Bartender** - License key
- [ ] **Dash** - License key
- [ ] **iStat Menus** - License key
- [ ] **Shortcat** - License key

## Account Logins

- [ ] **Dropbox** - Sign in (needed for Alfred sync, etc.)
- [ ] **NordVPN** - Sign in
- [ ] **Docker Desktop** - Sign in (optional, for Docker Hub)
- [ ] **Slack** - Sign in to workspaces
- [ ] **Discord** - Sign in
- [ ] **Zoom** - Sign in
- [ ] **Figma** - Sign in
- [ ] **Miro** - Sign in
- [ ] **Notion** - Sign in
- [ ] **Postman** - Sign in (optional)
- [ ] **Keybase** - Sign in or provision device

## Browsers

- [ ] **Brave** - Sign in to sync
- [ ] **Chrome** - Sign in to sync
- [ ] **Firefox** - Sign in to sync
- [ ] **Zen** - Configure as needed
- [ ] **Choosy** - Set as default browser, configure rules

## CLI Tools

```shell
gh auth login
fly auth login
gigalixir login
```

## Hardware-Specific

Only if you have the hardware:

- [ ] **CalDigit Docking Utility** - Configure dock
- [ ] **CalDigit Thunderbolt Charging** - Configure charging
- [ ] **Elgato Camera Hub** - Configure Facecam
- [ ] **Elgato Control Center** - Configure Key Light
- [ ] **Elgato Wave Link** - Configure Wave mic
- [ ] **Nanoleaf** - Discover lights
- [ ] **Doxie** - Configure scanner

## macOS Permissions

Grant when prompted (System Settings → Privacy & Security):

- [ ] **Accessibility** - Alfred, Bartender, Hammerspoon, Shortcat
- [ ] **Screen Recording** - Hammerspoon (for window management)
- [ ] **Automation** - Allow apps to control other apps

## App-Specific Setup

### Alfred
1. Move Spotlight hotkey to Option+Space (System Settings → Keyboard → Shortcuts → Spotlight)
2. Set Alfred hotkey to Cmd+Space
3. Point to Dropbox sync folder
4. Enable clipboard history

### Bartender
1. Arrange menu bar items
2. Set which items to hide

### Hammerspoon
1. Grant Accessibility permissions
2. Config already symlinked from dotfiles

### iStat Menus
1. Enter license
2. Configure menu bar items (CPU, memory, network)

### Ghostty
1. Config already symlinked from dotfiles
2. Set as default terminal if desired

## Optional

- [ ] **Aerial** - Set as screensaver in System Settings
- [ ] **Caffeine** - Enable launch at login
- [ ] **SelfControl** - Configure block list
- [ ] **Kiwi for Gmail** - Sign in to Google
- [ ] **Kindle** - Sign in to Amazon
- [ ] **Feedly** - Sign in
- [ ] **Twitter/X** - Sign in
