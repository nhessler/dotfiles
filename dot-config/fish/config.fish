# homebrew setup
set --global --prepend fish_user_paths "/opt/homebrew/sbin"
set --global --prepend fish_user_paths "/opt/homebrew/bin"
set --global --prepend fish_user_paths "/opt/homebrew/opt/curl/bin"
set --global --prepend fish_user_paths "/opt/homebrew/opt/postgresql@15/bin"

# monokai theme setup
source ~/.config/fish/color_syntax.fish

# asdf setup
source ~/.asdf/asdf.fish

# starship setup
starship init fish | source
