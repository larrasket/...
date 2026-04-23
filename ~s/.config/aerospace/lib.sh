#!/bin/bash
# Shared helpers for AeroSpace focus scripts
AEROSPACE=/opt/homebrew/bin/aerospace

# _find_window BUNDLE_PAT [TITLE_PAT]
# Prints: "wid ws" for the first matching window, or empty string
_find_window() {
    local bundle_pat="$1"
    local title_pat="${2:-}"

    local all
    all=$($AEROSPACE list-windows --all \
        --format '%{window-id} %{workspace} %{app-bundle-id} %{window-title}' 2>/dev/null \
        | grep -E "$bundle_pat")

    if [ -n "$title_pat" ]; then
        echo "$all" | grep "$title_pat" | head -1
    else
        echo "$all" | head -1
    fi
}

# focus_or_open TARGET_WS OPEN_CMD BUNDLE_PATTERN [TITLE_PATTERN]
#
# - TARGET_WS   workspace to move the window to (pass "" to skip moving)
# - OPEN_CMD    shell command to run when no window found (pass "" to do nothing)
# - BUNDLE_PAT  extended-regex matched against app-bundle-id
# - TITLE_PAT   (optional) plain string matched against window title
#
# If the app is running but hidden/minimized, open -a will un-hide it and
# the retry loop will catch the window once AeroSpace sees it.
focus_or_open() {
    local target_ws="$1"
    local open_cmd="$2"
    local bundle_pat="$3"
    local title_pat="${4:-}"

    local info
    info=$(_find_window "$bundle_pat" "$title_pat")

    if [ -z "$info" ] && [ -n "$open_cmd" ]; then
        # App not visible to AeroSpace (not running, hidden, or minimized).
        # activate/open it and switch to the target workspace optimistically.
        eval "$open_cmd"
        [ -n "$target_ws" ] && $AEROSPACE workspace "$target_ws"
        # Retry up to 10× (5 s total) waiting for the window to appear
        local i=0
        while [ $i -lt 10 ] && [ -z "$info" ]; do
            sleep 0.5
            info=$(_find_window "$bundle_pat" "$title_pat")
            i=$((i + 1))
        done
    fi

    if [ -n "$info" ]; then
        local wid ws
        wid=$(echo "$info" | awk '{print $1}')
        ws=$(echo "$info"  | awk '{print $2}')
        if [ -n "$target_ws" ] && [ "$ws" != "$target_ws" ]; then
            $AEROSPACE move-node-to-workspace --window-id "$wid" "$target_ws"
        fi
        [ -n "$target_ws" ] && $AEROSPACE workspace "$target_ws"
        $AEROSPACE focus --window-id "$wid"
    fi
}
