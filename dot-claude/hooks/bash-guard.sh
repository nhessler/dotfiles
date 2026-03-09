#!/bin/bash
# Claude Code PreToolUse hook: guards against bad Bash habits
# Move to: ~/.claude/hooks/bash-guard.sh

INPUT=$(cat)
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // empty')

# Guard 1: No && chaining — use separate tool calls
if echo "$COMMAND" | grep -qF '&&'; then
  echo "Blocked: Don't use && to chain commands. Use separate Bash tool calls instead." >&2
  exit 2
fi

# Guard 2: No cd or z — stay in the project root
if echo "$COMMAND" | grep -qE '(^|\s|;)(cd|z)\s'; then
  echo "Blocked: Don't use cd or z. Stay in the project root and use relative paths." >&2
  exit 2
fi

# Guard 3: No absolute paths (except /dev/null and /tmp)
if echo "$COMMAND" | grep -oE '(^|\s)/[^\s]+' | grep -vE '^(\s)*/dev/null' | grep -vE '^(\s)*/tmp' | grep -q .; then
  echo "Blocked: Don't use absolute paths. Use relative paths from the project root. (Exceptions: /dev/null, /tmp)" >&2
  exit 2
fi

exit 0
