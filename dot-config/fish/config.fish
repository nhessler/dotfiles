# homebrew setup
set --global --prepend fish_user_paths "/opt/homebrew/sbin"
set --global --prepend fish_user_paths "/opt/homebrew/bin"
set --global --prepend fish_user_paths "/opt/homebrew/opt/curl/bin"

# monokai theme setup
source ~/.config/fish/color_syntax.fish

# direnv setup
direnv hook fish | source

# zoxide setup
zoxide init fish | source

# asdf setup
# source ~/.asdf/asdf.fish

# ASDF configuration code
if test -z $ASDF_DATA_DIR
    set _asdf_shims "$HOME/.asdf/shims"
else
    set _asdf_shims "$ASDF_DATA_DIR/shims"
end

# Do not use fish_add_path (added in Fish 3.2) because it
# potentially changes the order of items in PATH
if not contains $_asdf_shims $PATH
    set -gx --prepend PATH $_asdf_shims
end
set --erase _asdf_shims

# Don't forget to manually link asdf completions
# $> ln -s ~/.asdf/completions/asdf.fish ~/.config/fish/completions

# asdf-ruby: auto-install these gems with each new Ruby version
set -x ASDF_GEM_DEFAULT_PACKAGES_FILE "$HOME/.config/.default_gems"

# Enable IEx shell history
# set -Ux ERL_AFLAGS '-kernel shell_history enabled'

# Erlang config options
# set -Ux KERL_CONFIGURE_OPTIONS '--without-javac --without-jinterface --without-odbc --without-hipe --with-wx-config=/opt/homebrew/bin/wx-config'
# set -Ux KERL_BUILD_DOCS 'yes'

# starship setup
starship init fish | source

# fzf setup
fzf --fish | source

# for future potential ENV_VAR needs -- https://stackoverflow.com/questions/25632846/how-to-set-environment-variables-in-fish-shell

# set gpg to look at terminal for passphrase
set -x GPG_TTY $(tty)

#set gigalixir identity path
set -Ux GIGALIXIR_IDENTITY_FILE "$HOME/.ssh/id_ed25519.gigalixir.convivialtech"

# setup direnv
direnv hook fish | source

set -g direnv_fish_mode eval_on_arrow    # trigger direnv at prompt, and on every arrow-based directory change (default)
# set -g direnv_fish_mode eval_after_arrow # trigger direnv at prompt, and only after arrow-based directory changes before executing command
# set -g direnv_fish_mode disable_arrow    # trigger direnv at prompt only, this is similar functionality to the original behavior
