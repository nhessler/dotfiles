# homebrew setup
set --global --prepend fish_user_paths "/opt/homebrew/sbin"
set --global --prepend fish_user_paths "/opt/homebrew/bin"
set --global --prepend fish_user_paths "/opt/homebrew/opt/curl/bin"
set --global --prepend fish_user_paths "/opt/homebrew/opt/postgresql@15/bin"

# custom git commands
set --global --append fish_user_paths "$HOME/.gitbin"

# monokai theme setup
source ~/.config/fish/color_syntax.fish

# asdf setup
source ~/.asdf/asdf.fish

# Don't forget to manually link asdf completions 
# $> ln -s ~/.asdf/completions/asdf.fish ~/.config/fish/completions

# Enable IEx shell history
# set -Ux ERL_AFLAGS '-kernel shell_history enabled'

# Erlang config options
# set -Ux KERL_CONFIGURE_OPTIONS '--without-javac --without-jinterface --without-odbc --without-hipe --with-wx-config=/opt/homebrew/bin/wx-config'
# set -Ux KERL_BUILD_DOCS 'yes'

# starship setup
starship init fish | source

# direnv setup
direnv hook fish | source

# fzf setup
fzf --fish | source

# for future potential ENV_VAR needs -- https://stackoverflow.com/questions/25632846/how-to-set-environment-variables-in-fish-shell

# set gpg to look at terminal for passphrase
set -x GPG_TTY $(tty)

# set -g direnv_fish_mode eval_on_arrow    # trigger direnv at prompt, and on every arrow-based directory change (default)
# set -g direnv_fish_mode eval_after_arrow # trigger direnv at prompt, and only after arrow-based directory changes before executing command
# set -g direnv_fish_mode disable_arrow    # trigger direnv at prompt only, this is similar functionality to the original behavior

