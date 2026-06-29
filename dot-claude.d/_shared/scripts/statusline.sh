#!/usr/bin/env bash
#
# Claude Code statusline — shared across all nh accounts.
# Wired via settings.json:  "statusLine": { "command": "$CLAUDE_CONFIG_DIR/scripts/statusline.sh" }
# Reads the statusline JSON payload on stdin, prints one formatted line.
#
# Layout:  project on branch | account | model | <context bar> | 5h <used>%·<min>m | 7d <used>%·<hr>h

input=$(cat)

# Opt-in payload capture for schema debugging:  export CLAUDE_STATUSLINE_DEBUG=1
[ -n "$CLAUDE_STATUSLINE_DEBUG" ] && echo "$input" >> /tmp/claude-status-debug.log

# ─── Colors ──────────────────────────────────────────────────────────────────
RESET='\033[0m'
SEP='\033[97m|\033[0m'        # bright-white pipe separator
C_PROJECT='\033[1;36m'        # cyan
C_ON='\033[1;97m'             # white "on"
C_BRANCH='\033[1;95m'         # magenta
C_ACCOUNT='\033[1;31m'        # red
C_MODEL='\033[1;34m'          # blue
C_GREEN='\033[1;92m'
C_YELLOW='\033[1;93m'
C_RED='\033[1;91m'

# Color a value by how "used" it is (low = green, high = red).
usage_color() {
    local pct=$1
    if [ "$pct" -ge 80 ]; then echo "$C_RED"
    elif [ "$pct" -ge 50 ]; then echo "$C_YELLOW"
    else echo "$C_GREEN"
    fi
}

# ─── Parse payload ───────────────────────────────────────────────────────────
current_dir=$(echo "$input" | jq -r ".workspace.current_dir")
model=$(echo "$input" | jq -r ".model.display_name")
window_size=$(echo "$input" | jq -r ".context_window.context_window_size")
current_usage=$(echo "$input" | jq ".context_window.current_usage")

# Account = basename of the active config dir (falls back to nh).
if [ -n "$CLAUDE_CONFIG_DIR" ]; then
    account=$(basename "$CLAUDE_CONFIG_DIR")
else
    account="nh"
fi

# Project = <org>/<repo> when under ~/Projects, else the dir's basename.
if [[ "$current_dir" =~ /Users/nathan/Projects/([^/]+)/([^/]+) ]]; then
    project="${BASH_REMATCH[1]}/${BASH_REMATCH[2]}"
else
    project=$(basename "$current_dir")
fi

# Git branch (if inside a work tree).
if git -C "$current_dir" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    branch=$(git -C "$current_dir" branch --show-current 2>/dev/null || echo "detached")
    git_info=" ${C_ON}on${RESET} ${C_BRANCH}${branch}${RESET}"
else
    git_info=""
fi

# ─── Context window bar (colored by remaining headroom) ──────────────────────
if [ "$current_usage" != "null" ]; then
    input_tokens=$(echo "$current_usage" | jq ".input_tokens // 0")
    cache_creation=$(echo "$current_usage" | jq ".cache_creation_input_tokens // 0")
    cache_read=$(echo "$current_usage" | jq ".cache_read_input_tokens // 0")
    current_tokens=$((input_tokens + cache_creation + cache_read))
    percent_used=$((current_tokens * 100 / window_size))
    remaining_percent=$((100 - percent_used))

    if [ $remaining_percent -lt 30 ]; then ctx_color="$C_RED"
    elif [ $remaining_percent -le 40 ]; then ctx_color="$C_YELLOW"
    else ctx_color="$C_GREEN"
    fi

    bar_length=10
    filled=$((remaining_percent * bar_length / 100))
    bar=""
    i=0
    while [ $i -lt $filled ]; do bar="${bar}█"; i=$((i + 1)); done
    while [ $i -lt $bar_length ]; do bar="${bar}░"; i=$((i + 1)); done
    context_display="${ctx_color}${bar} ${remaining_percent}%${RESET}"
else
    context_display="${C_GREEN}██████████ 100%${RESET}"
fi

# ─── Rate-limit windows (5h / 7d): % used · time until reset ─────────────────
now=$(date +%s)
rate_display=""

five_pct=$(echo "$input" | jq -r ".rate_limits.five_hour.used_percentage // empty")
five_reset=$(echo "$input" | jq -r ".rate_limits.five_hour.resets_at // empty")
if [ -n "$five_pct" ] && [ -n "$five_reset" ]; then
    five_min=$(( (five_reset - now) / 60 )); [ $five_min -lt 0 ] && five_min=0
    fc=$(usage_color "$five_pct")
    rate_display="${rate_display} ${SEP} ${fc}5h ${five_pct}%·${five_min}m${RESET}"
fi

seven_pct=$(echo "$input" | jq -r ".rate_limits.seven_day.used_percentage // empty")
seven_reset=$(echo "$input" | jq -r ".rate_limits.seven_day.resets_at // empty")
if [ -n "$seven_pct" ] && [ -n "$seven_reset" ]; then
    seven_hr=$(( (seven_reset - now) / 3600 )); [ $seven_hr -lt 0 ] && seven_hr=0
    sc=$(usage_color "$seven_pct")
    rate_display="${rate_display} ${SEP} ${sc}7d ${seven_pct}%·${seven_hr}h${RESET}"
fi

# ─── Render ──────────────────────────────────────────────────────────────────
echo -e "${C_PROJECT}${project}${RESET}${git_info} ${SEP} ${C_ACCOUNT}${account}${RESET} ${SEP} ${C_MODEL}${model}${RESET} ${SEP} ${context_display}${rate_display}"
