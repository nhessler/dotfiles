# homebrew setup
set --global --prepend fish_user_paths "/opt/homebrew/sbin"
set --global --prepend fish_user_paths "/opt/homebrew/bin"

# monokai theme setup
source ~/.config/fish/color_syntax.fish

# asdf setup
source ~/.asdf/asdf.fish

# starship setup
starship init fish | source