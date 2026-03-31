function _nh_sync -d "Interactively reconcile installed vs tracked packages"
    if contains -- --help $argv; or contains -- -h $argv
        echo "Usage: nh sync [keep|remove|reset|skipped] [-o|--only category]"
        echo ""
        echo "Reconcile installed packages vs tracked config files."
        echo ""
        echo "Subcommands:"
        echo "  keep      Find installed-but-not-tracked items (default)"
        echo "  remove    Find tracked-but-not-installed items"
        echo "  skipped   Show currently skipped items"
        echo "  reset     Clear skip list"
        echo ""
        echo "Options:"
        echo "  -o, --only <cat>  Only run one category: brew, cask, mas, asdf, emacs"
        echo ""
        echo "Categories checked: brew formulae, casks, MAS, ASDF, Emacs"
        return 0
    end

    # Parse subcommand
    set -l mode keep
    set -l remaining $argv
    if test (count $remaining) -gt 0
        switch $remaining[1]
            case keep
                set mode keep
                set remaining $remaining[2..-1]
            case remove
                set mode remove
                set remaining $remaining[2..-1]
            case skipped
                _nh_ensure_state_dir
                set -l skip_only ""
                for i in (seq 2 (count $remaining))
                    if begin; test "$remaining[$i]" = "-o"; or test "$remaining[$i]" = "--only"; end
                        if test (count $remaining) -ge (math $i + 1)
                            set skip_only $remaining[(math $i + 1)]
                        end
                    end
                end
                _nh_sync_show_skips $skip_only
                return 0
            case reset
                _nh_sync_clear_skips
                echo "Skip list cleared. Run 'nh sync' to re-evaluate."
                return 0
        end
    end

    # Parse -o/--only flag
    set -l only ""
    if test (count $remaining) -ge 1
        for i in (seq 1 (count $remaining))
            if begin; test "$remaining[$i]" = "-o"; or test "$remaining[$i]" = "--only"; end
                if test (count $remaining) -ge (math $i + 1)
                    set only $remaining[(math $i + 1)]
                end
            end
        end
    end

    _nh_ensure_state_dir

    if test -z "$only" -o "$only" = brew
        echo "=== Brew Formulae ==="
        _nh_sync_brews $mode; or return
        echo ""
    end
    if test -z "$only" -o "$only" = cask
        echo "=== Brew Casks ==="
        _nh_sync_casks $mode; or return
        echo ""
    end
    if test -z "$only" -o "$only" = cask; and test $mode = keep
        echo "=== Nerd Fonts (informational) ==="
        _nh_sync_nerd_fonts
        echo ""
    end
    if test -z "$only" -o "$only" = mas
        echo "=== Mac App Store ==="
        _nh_sync_mas $mode; or return
        echo ""
    end
    if test -z "$only" -o "$only" = asdf
        echo "=== ASDF Plugins ==="
        _nh_sync_asdf $mode; or return
        echo ""
    end
    if test -z "$only" -o "$only" = emacs
        echo "=== Emacs Packages (informational) ==="
        _nh_sync_emacs
        echo ""
    end
end

# --- Skip list helpers ---

function _nh_sync_skip_file
    echo (_nh_state_dir)/sync-skips
end

function _nh_sync_is_skipped
    set -l type $argv[1]
    set -l item $argv[2]
    set -l skip_file (_nh_sync_skip_file)
    test -f $skip_file; and grep -qxF "$type:$item" $skip_file
end

function _nh_sync_add_skip
    set -l type $argv[1]
    set -l item $argv[2]
    set -l skip_file (_nh_sync_skip_file)
    set -l new_entry "$type:$item"
    set -l section_order brew cask mas asdf emacs

    # Read existing entries
    set -l entries
    if test -f $skip_file
        set entries (cat $skip_file)
    end

    # Add the new entry
    set -a entries $new_entry

    # Write back sorted by section order, then alphabetically within section
    set -l tmpfile (mktemp)
    for section in $section_order
        for entry in (printf '%s\n' $entries | grep "^$section:" | sort)
            echo $entry >> $tmpfile
        end
    end
    mv $tmpfile $skip_file
end

function _nh_sync_clear_skips
    set -l skip_file (_nh_sync_skip_file)
    if test -f $skip_file
        rm $skip_file
    end
end

function _nh_sync_show_skips
    set -l only $argv[1]
    set -l skip_file (_nh_sync_skip_file)
    if not test -f $skip_file; or test (wc -l < $skip_file | string trim) -eq 0
        echo "=== Skipped Items ==="
        echo "  No items skipped"
        return 0
    end

    set -l section_labels brew Brews cask Casks mas "App Store" asdf ASDF emacs Emacs

    # Cache mas list for name lookups
    set -l mas_cache (mas list 2>/dev/null)

    echo "=== Skipped Items ==="
    for i in (seq 1 2 (count $section_labels))
        set -l section $section_labels[$i]
        set -l label $section_labels[(math $i + 1)]
        # Skip sections not matching -o filter
        if test -n "$only"; and test "$only" != "$section"
            continue
        end
        set -l items (grep "^$section:" $skip_file | string replace "$section:" "")
        if test (count $items) -gt 0
            echo "  $label:"
            for item in $items
                if test $section = mas
                    # Look up app name from mas list
                    set -l mas_line (printf '%s\n' $mas_cache | string match -r "^\s*$item\s+.*" | head -1)
                    set -l name (echo $mas_line | string replace -r '^\s*\d+\s+' '' | string replace -r '\s+\(.*\)$' '')
                    if test -n "$name"
                        echo "    $name ($item)"
                    else
                        echo "    $item"
                    end
                else
                    echo "    $item"
                end
            end
        end
    end
    echo ""
    echo "  To re-prompt for these, run: nh sync --reset"
    echo ""
end

# --- Interactive prompt helpers ---

function _nh_sync_prompt_keep
    set -l item $argv[1]
    read -P "  $item [k]eep / [s]kip / [q]uit? " -l answer
    switch (string lower $answer)
        case '' k
            echo keep
        case s
            echo skip
        case q
            echo quit
        case '*'
            echo skip
    end
end

function _nh_sync_prompt_remove
    set -l item $argv[1]
    read -P "  $item [r]emove / [k]eep / [q]uit? " -l answer
    switch (string lower $answer)
        case '' r
            echo remove
        case k
            echo keep
        case q
            echo quit
        case '*'
            echo keep
    end
end

# --- Brewfile helpers ---

function _nh_sync_brewfile_path
    echo "$HOME/.config/homebrew/Brewfile"
end

function _nh_sync_brewfile_add
    set -l section_prefix $argv[1]
    set -l new_line $argv[2]
    set -l brewfile (_nh_sync_brewfile_path)
    set -l tmpfile (mktemp)

    set -l in_section 0
    set -l inserted 0
    set -l past_section 0

    while read -l line
        # Detect if we're in the target section
        if string match -qr "^$section_prefix " "$line"
            set in_section 1
            # Insert before this line if new_line sorts before it and not yet inserted
            if test $inserted -eq 0
                # Fish string comparison: first in sort order wins
                set -l sorted (printf '%s\n' "$new_line" "$line" | sort)
                if test "$sorted[1]" = "$new_line"; and test "$new_line" != "$line"
                    printf '%s\n' "$new_line" >> $tmpfile
                    set inserted 1
                end
            end
        else if test $in_section -eq 1
            # We just left the section — insert at end of section if not yet inserted
            if test $inserted -eq 0
                printf '%s\n' "$new_line" >> $tmpfile
                set inserted 1
            end
            set in_section 0
            set past_section 1
        end
        printf '%s\n' "$line" >> $tmpfile
    end < $brewfile

    # Handle case where section is at end of file
    if test $inserted -eq 0; and test $in_section -eq 1
        printf '%s\n' "$new_line" >> $tmpfile
    end

    # Handle case where section doesn't exist yet
    if test $inserted -eq 0; and test $in_section -eq 0; and test $past_section -eq 0
        printf '\n' >> $tmpfile
        printf '%s\n' "$new_line" >> $tmpfile
    end

    mv $tmpfile $brewfile
end

function _nh_sync_brewfile_remove
    set -l pattern $argv[1]
    set -l brewfile (_nh_sync_brewfile_path)
    set -l tmpfile (mktemp)

    while read -l line
        if not string match -qr "$pattern" "$line"
            printf '%s\n' "$line" >> $tmpfile
        end
    end < $brewfile

    mv $tmpfile $brewfile
end

# --- Brew formulae sync ---

function _nh_sync_brews
    set -l mode $argv[1]
    set -l brewfile (_nh_sync_brewfile_path)

    set -l tracked (string match -r '^brew "(.+)"' < $brewfile | string match -v -r '^brew')

    if test $mode = keep
        # Use brew leaves for keep mode — only top-level packages
        set -l installed (brew leaves 2>/dev/null)
        set -l untracked
        for formula in $installed
            if not contains $formula $tracked; and not _nh_sync_is_skipped brew $formula
                set -a untracked $formula
            end
        end

        if test (count $untracked) -eq 0
            echo "  Everything in sync"
            return 0
        end

        for formula in $untracked
            set -l answer (_nh_sync_prompt_keep $formula)
            switch $answer
                case keep
                    _nh_sync_brewfile_add brew "brew \"$formula\""
                    echo "    Added to Brewfile"
                case skip
                    _nh_sync_add_skip brew $formula
                    echo "    Skipped"
                case quit
                    return 1
            end
        end
    else if test $mode = remove
        # Use brew list for remove mode — all installed including dependencies
        set -l all_installed (brew list --formula 2>/dev/null)

        set -l not_installed
        for formula in $tracked
            if not contains $formula $all_installed
                set -a not_installed $formula
            end
        end

        if test (count $not_installed) -eq 0
            echo "  Everything in sync"
            return 0
        end

        for formula in $not_installed
            set -l answer (_nh_sync_prompt_remove $formula)
            switch $answer
                case remove
                    _nh_sync_brewfile_remove "^brew \"$formula\"\$"
                    echo "    Removed from Brewfile"
                case keep
                    echo "    Kept"
                case quit
                    return 1
            end
        end
    end
end

# --- Brew casks sync ---

function _nh_sync_casks
    set -l mode $argv[1]
    set -l brewfile (_nh_sync_brewfile_path)

    # Exclude nerd font casks — managed by bin/install-nerd-fonts.sh
    # Regex from: https://gist.github.com/davidteren/898f2dcccd42d9f8680ec69a3a5d350e
    set -l installed (brew list --cask 2>/dev/null | string match -v -r '^font-.*(nerd-font|nerd$|-nf$|-nf-)')
    # Strip tap prefix from tracked names (e.g., "d12frosted/emacs-plus/emacs-plus-app" → "emacs-plus-app")
    set -l tracked (string match -r '^cask "(.+)"' < $brewfile | string match -v -r '^cask' | string replace -r '.+/' '')

    if test $mode = keep
        set -l untracked
        for cask in $installed
            if not contains $cask $tracked; and not _nh_sync_is_skipped cask $cask
                set -a untracked $cask
            end
        end

        if test (count $untracked) -eq 0
            echo "  Everything in sync"
            return 0
        end

        for cask in $untracked
            set -l answer (_nh_sync_prompt_keep $cask)
            switch $answer
                case keep
                    _nh_sync_brewfile_add cask "cask \"$cask\""
                    echo "    Added to Brewfile"
                case skip
                    _nh_sync_add_skip cask $cask
                    echo "    Skipped"
                case quit
                    return 1
            end
        end
    else if test $mode = remove
        set -l not_installed
        for cask in $tracked
            if not contains $cask $installed
                set -a not_installed $cask
            end
        end

        if test (count $not_installed) -eq 0
            echo "  Everything in sync"
            return 0
        end

        for cask in $not_installed
            set -l answer (_nh_sync_prompt_remove $cask)
            switch $answer
                case remove
                    _nh_sync_brewfile_remove "^cask \".*$cask\"\$"
                    echo "    Removed from Brewfile"
                case keep
                    echo "    Kept"
                case quit
                    return 1
            end
        end
    end
end

# --- Nerd fonts (informational) ---

function _nh_sync_nerd_fonts
    # Same regex as bin/install-nerd-fonts.sh
    # Based on: https://gist.github.com/davidteren/898f2dcccd42d9f8680ec69a3a5d350e
    set -l available (brew search '/font-.*(nerd-font|nerd$|-nf$|-nf-)/' 2>/dev/null | awk '{ print $1 }')
    set -l installed (brew list --cask 2>/dev/null | string match -r '^font-.*(nerd-font|nerd$|-nf$|-nf-)')

    set -l not_installed
    for font in $available
        if not contains $font $installed
            set -a not_installed $font
        end
    end

    if test (count $not_installed) -eq 0
        echo "  All "(count $available)" nerd fonts installed"
    else
        echo "  "(count $not_installed)" of "(count $available)" nerd fonts not installed:"
        for font in $not_installed
            echo "    $font"
        end
        echo ""
        echo "  Run: bin/install-nerd-fonts.sh"
    end
end

# --- MAS sync ---

function _nh_sync_mas
    set -l mode $argv[1]
    set -l brewfile (_nh_sync_brewfile_path)

    # Parse installed: "ID  Name (version)" → extract ID and name
    set -l installed_ids
    set -l installed_names
    for line in (mas list 2>/dev/null)
        set -l id (echo $line | string match -r '^\s*(\d+)' | tail -1)
        set -l name (echo $line | string replace -r '^\s*\d+\s+' '' | string replace -r '\s+\(.*\)$' '')
        if test -n "$id"
            set -a installed_ids $id
            set -a installed_names $name
        end
    end

    # Parse tracked IDs from Brewfile
    set -l tracked_ids (grep '^mas ' $brewfile | string match -r 'id:\s*(\d+)' | string match -r '^\d+$')

    if test $mode = keep
        set -l untracked_indices
        for i in (seq (count $installed_ids))
            set -l id $installed_ids[$i]
            if not contains $id $tracked_ids; and not _nh_sync_is_skipped mas $id
                set -a untracked_indices $i
            end
        end

        if test (count $untracked_indices) -eq 0
            echo "  Everything in sync"
            return 0
        end

        for i in $untracked_indices
            set -l id $installed_ids[$i]
            set -l name $installed_names[$i]
            set -l answer (_nh_sync_prompt_keep "$name ($id)")
            switch $answer
                case keep
                    _nh_sync_brewfile_add_mas $name $id
                    echo "    Added to Brewfile"
                case skip
                    _nh_sync_add_skip mas $id
                    echo "    Skipped"
                case quit
                    return 1
            end
        end
    else if test $mode = remove
        # Parse tracked names and IDs from Brewfile
        set -l tracked_names (string match -r '^mas "([^"]+)"' < $brewfile | string match -v -r '^mas')
        set -l tracked_id_list (grep '^mas ' $brewfile | string match -r 'id:\s*(\d+)' | string match -r '^\d+$')

        set -l not_installed
        set -l not_installed_names
        for i in (seq (count $tracked_id_list))
            set -l id $tracked_id_list[$i]
            if not contains $id $installed_ids
                set -a not_installed $id
                set -a not_installed_names $tracked_names[$i]
            end
        end

        if test (count $not_installed) -eq 0
            echo "  Everything in sync"
            return 0
        end

        for i in (seq (count $not_installed))
            set -l id $not_installed[$i]
            set -l name $not_installed_names[$i]
            set -l answer (_nh_sync_prompt_remove "$name ($id)")
            switch $answer
                case remove
                    _nh_sync_brewfile_remove "id:\\s*$id"
                    echo "    Removed from Brewfile"
                case keep
                    echo "    Kept"
                case quit
                    return 1
            end
        end
    end
end

function _nh_sync_brewfile_add_mas
    set -l name $argv[1]
    set -l id $argv[2]
    set -l brewfile (_nh_sync_brewfile_path)
    set -l tmpfile (mktemp)

    set -l new_prefix "mas \"$name\","
    set -l new_prefix_len (string length "$new_prefix")

    # Detect alignment column from existing entries by finding where "id:" starts
    set -l align_col 0
    while read -l line
        if string match -qr '^mas ' "$line"
            set -l before (echo "$line" | string split "id:" | head -1)
            set -l col (string length "$before")
            if test $col -gt $align_col
                set align_col $col
            end
        end
    end < $brewfile

    # If new entry's prefix is longer, bump the alignment column
    # +1 for at least one space after comma
    if test (math $new_prefix_len + 1) -gt $align_col
        set align_col (math $new_prefix_len + 1)
    end

    # If no existing entries, use prefix + reasonable padding
    if test $align_col -eq 0
        set align_col (math $new_prefix_len + 1)
    end

    set -l needs_repad (test (math $new_prefix_len + 1) -ge $align_col; and echo 1; or echo 0)
    set -l in_section 0
    set -l inserted 0

    while read -l line
        if string match -qr '^mas ' "$line"
            set in_section 1

            # Extract name and id from existing entry
            set -l existing_name (echo "$line" | string match -r '^mas "([^"]+)"' | tail -1)
            set -l existing_id (echo "$line" | string match -r 'id:\s*(\d+)' | tail -1)

            # Repad existing entry if alignment changed
            if test $needs_repad -eq 1; and test -n "$existing_name" -a -n "$existing_id"
                set -l ep "mas \"$existing_name\","
                set -l pad_len (math $align_col - (string length "$ep"))
                if test $pad_len -lt 1
                    set pad_len 1
                end
                set -l padding (string repeat -n $pad_len ' ')
                set line "$ep$padding""id: $existing_id"
            end

            # Insert new entry in sorted position
            if test $inserted -eq 0
                set -l existing_name_lower (string lower "$existing_name")
                set -l new_name_lower (string lower "$name")
                set -l sorted (printf '%s\n' "$new_name_lower" "$existing_name_lower" | sort)
                if test "$sorted[1]" = "$new_name_lower"; and test "$new_name_lower" != "$existing_name_lower"
                    set -l pad_len (math $align_col - $new_prefix_len)
                    if test $pad_len -lt 1
                        set pad_len 1
                    end
                    set -l padding (string repeat -n $pad_len ' ')
                    printf '%s\n' "$new_prefix$padding""id: $id" >> $tmpfile
                    set inserted 1
                end
            end
        else if test $in_section -eq 1
            if test $inserted -eq 0
                set -l pad_len (math $align_col - $new_prefix_len)
                if test $pad_len -lt 1
                    set pad_len 1
                end
                set -l padding (string repeat -n $pad_len ' ')
                printf '%s\n' "$new_prefix$padding""id: $id" >> $tmpfile
                set inserted 1
            end
            set in_section 0
        end
        printf '%s\n' "$line" >> $tmpfile
    end < $brewfile

    # Handle mas section at end of file
    if test $inserted -eq 0; and test $in_section -eq 1
        set -l pad_len (math $align_col - $new_prefix_len)
        if test $pad_len -lt 1
            set pad_len 1
        end
        set -l padding (string repeat -n $pad_len ' ')
        printf '%s\n' "$new_prefix$padding""id: $id" >> $tmpfile
    end

    mv $tmpfile $brewfile
end

# --- ASDF sync ---

function _nh_sync_asdf_plugins_file
    echo "$HOME/.config/asdf/plugins"
end

function _nh_sync_asdf_add
    set -l plugin $argv[1]
    set -l plugins_file (_nh_sync_asdf_plugins_file)
    echo $plugin >> $plugins_file
    sort -o $plugins_file $plugins_file
end

function _nh_sync_asdf_remove
    set -l plugin $argv[1]
    set -l plugins_file (_nh_sync_asdf_plugins_file)
    set -l tmpfile (mktemp)
    grep -vxF $plugin $plugins_file > $tmpfile
    mv $tmpfile $plugins_file
end

function _nh_sync_asdf
    set -l mode $argv[1]
    set -l plugins_file (_nh_sync_asdf_plugins_file)

    if not command -q asdf
        echo "  asdf not installed"
        return 0
    end

    set -l installed (asdf plugin list 2>/dev/null)
    set -l tracked
    if test -f $plugins_file
        set tracked (cat $plugins_file | string match -v -r '^\s*$')
    end

    if test $mode = keep
        set -l untracked
        for plugin in $installed
            if not contains $plugin $tracked; and not _nh_sync_is_skipped asdf $plugin
                set -a untracked $plugin
            end
        end

        if test (count $untracked) -eq 0
            echo "  Everything in sync"
            return 0
        end

        for plugin in $untracked
            set -l answer (_nh_sync_prompt_keep $plugin)
            switch $answer
                case keep
                    _nh_sync_asdf_add $plugin
                    echo "    Added to plugins file"
                case skip
                    _nh_sync_add_skip asdf $plugin
                    echo "    Skipped"
                case quit
                    return 1
            end
        end
    else if test $mode = remove
        set -l not_installed
        for plugin in $tracked
            if not contains $plugin $installed
                set -a not_installed $plugin
            end
        end

        if test (count $not_installed) -eq 0
            echo "  Everything in sync"
            return 0
        end

        for plugin in $not_installed
            set -l answer (_nh_sync_prompt_remove $plugin)
            switch $answer
                case remove
                    _nh_sync_asdf_remove $plugin
                    echo "    Removed from plugins file"
                case keep
                    echo "    Kept"
                case quit
                    return 1
            end
        end
    end
end

# --- Emacs drift detection (informational) ---

function _nh_sync_emacs
    if not command -q emacs
        echo "  emacs not installed"
        return 0
    end

    set -l emacs_dir "$HOME/.config/emacs"

    # Built-in packages (use-package with :ensure nil or known built-ins)
    set -l builtins recentf saveplace uniquify time eglot flymake eldoc org \
        css-mode js typescript-ts-mode json-ts-mode sh-script conf-mode \
        ruby-mode ielm erlang heex-ts-mode

    # Get installed packages from elpa
    set -l installed (emacs --batch \
        --eval '(package-initialize)' \
        --eval '(dolist (pkg package-alist) (message "%s" (car pkg)))' \
        2>&1 | grep -v '^\s*$' | sort)

    # Get declared packages from use-package
    set -l declared (grep -h '(use-package ' \
        $emacs_dir/init.el $emacs_dir/lisp/*.el $emacs_dir/languages/*.el \
        2>/dev/null | grep -v '^\s*;' | \
        sed 's/.*use-package //' | sed 's/[) ].*//' | sort -u)

    # Filter out built-ins from declared
    set -l declared_external
    for pkg in $declared
        if not contains $pkg $builtins
            set -a declared_external $pkg
        end
    end

    # Get packages that are dependencies of other packages
    set -l deps (emacs --batch \
        --eval '(package-initialize)' \
        --eval '(dolist (pkg package-alist) (let ((reqs (package-desc-reqs (cadr pkg)))) (dolist (req reqs) (message "%s" (car req)))))' \
        2>&1 | grep -v '^\s*$' | sort -u)

    # Installed but not declared (excluding dependencies)
    set -l undeclared
    for pkg in $installed
        if not contains $pkg $declared_external; and not contains $pkg $deps
            set -a undeclared $pkg
        end
    end

    # Declared but not installed
    set -l missing
    for pkg in $declared_external
        if not contains $pkg $installed
            set -a missing $pkg
        end
    end

    if test (count $undeclared) -eq 0; and test (count $missing) -eq 0
        echo "  Everything in sync"
        return 0
    end

    if test (count $undeclared) -gt 0
        echo "  Installed but not declared ("(count $undeclared)"):"
        for pkg in $undeclared
            echo "    $pkg"
        end
    end

    if test (count $missing) -gt 0
        echo "  Declared but not installed ("(count $missing)"):"
        for pkg in $missing
            echo "    $pkg"
        end
    end

    echo ""
    echo "  (Manual action required — edit Emacs config files directly)"
end
