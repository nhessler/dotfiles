# Wrap `claude` so CLAUDE_CONFIG_DIR is auto-set from the current directory's
# account — the same cwd routing `nh claude` uses. This makes ad-hoc commands
# (`claude mcp add`, `claude config`, etc.) land on the right account without
# the `env CLAUDE_CONFIG_DIR=…` dance.
#
# Only injects the var when it isn't already set, so:
#   - an explicit `set -x CLAUDE_CONFIG_DIR …` or `env … claude` still wins
#   - inside a running session's shell (var already exported) it's respected
#   - `nh claude` is unaffected (it calls the binary via `env`, bypassing this)
#
# Falls back to the `nh` account when cwd doesn't map to anything.

function claude --description "claude with cwd-based account routing (sets CLAUDE_CONFIG_DIR)"
    if set -q CLAUDE_CONFIG_DIR
        command claude $argv
        return $status
    end

    set -l account (_nh_account_for_cwd)
    test -z "$account"; and set account nh

    set -lx CLAUDE_CONFIG_DIR (_nh_account_config_dir $account)
    command claude $argv
end
