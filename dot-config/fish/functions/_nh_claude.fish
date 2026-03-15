function _nh_claude -d "Open Claude Code in a named Ghostty tab"
    set -l folder_name (basename (pwd))
    claude --continue --name "$folder_name - CC"
end
