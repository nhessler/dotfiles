function _nh_sync -d "Interactively reconcile installed vs tracked packages"
    if contains -- --help $argv; or contains -- -h $argv
        echo "Usage: nh sync [--remove] [--reset]"
        echo ""
        echo "Reconcile installed packages vs tracked config files."
        echo ""
        echo "Modes:"
        echo "  (default)   Find installed-but-not-tracked items (keep/skip)"
        echo "  --remove    Find tracked-but-not-installed items (remove/keep)"
        echo "  --reset     Clear skip list, then run default sync"
        echo ""
        echo "Categories checked: brew formulae, casks, MAS, ASDF, Emacs"
        return 0
    end

    set -l mode keep
    if contains -- --remove $argv
        set mode remove
    else if contains -- --reset $argv
        _nh_sync_clear_skips
        echo "Skip list cleared."
        echo ""
    end

    _nh_ensure_state_dir

    echo "=== Brew Formulae ==="
    _nh_sync_brews $mode; or return
    echo ""
    echo "=== Brew Casks ==="
    _nh_sync_casks $mode; or return
    echo ""
    echo "=== Nerd Fonts (informational) ==="
    _nh_sync_nerd_fonts
    echo ""
    echo "=== Mac App Store ==="
    echo "  (not yet implemented)"
    echo ""
    echo "=== ASDF Plugins ==="
    echo "  (not yet implemented)"
    echo ""
    echo "=== Emacs Packages (informational) ==="
    echo "  (not yet implemented)"
    echo ""

    _nh_sync_show_skips
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
    set -l skip_file (_nh_sync_skip_file)
    if not test -f $skip_file; or test (wc -l < $skip_file | string trim) -eq 0
        return 0
    end

    set -l section_labels brew Brews cask Casks mas "App Store" asdf ASDF emacs Emacs

    echo "=== Skipped Items ==="
    for i in (seq 1 2 (count $section_labels))
        set -l section $section_labels[$i]
        set -l label $section_labels[(math $i + 1)]
        set -l items (grep "^$section:" $skip_file | string replace "$section:" "")
        if test (count $items) -gt 0
            echo "  $label:"
            for item in $items
                echo "    $item"
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

    for line in (cat $brewfile)
        # Detect if we're in the target section
        if string match -qr "^$section_prefix " $line
            set in_section 1
            # Insert before this line if new_line sorts before it and not yet inserted
            if test $inserted -eq 0
                # Fish string comparison: first in sort order wins
                set -l sorted (printf '%s\n' $new_line $line | sort)
                if test "$sorted[1]" = "$new_line"; and test "$new_line" != "$line"
                    echo $new_line >> $tmpfile
                    set inserted 1
                end
            end
        else if test $in_section -eq 1
            # We just left the section — insert at end of section if not yet inserted
            if test $inserted -eq 0
                echo $new_line >> $tmpfile
                set inserted 1
            end
            set in_section 0
            set past_section 1
        end
        echo $line >> $tmpfile
    end

    # Handle case where section is at end of file
    if test $inserted -eq 0; and test $in_section -eq 1
        echo $new_line >> $tmpfile
    end

    # Handle case where section doesn't exist yet
    if test $inserted -eq 0; and test $in_section -eq 0; and test $past_section -eq 0
        echo "" >> $tmpfile
        echo $new_line >> $tmpfile
    end

    mv $tmpfile $brewfile
end

function _nh_sync_brewfile_remove
    set -l pattern $argv[1]
    set -l brewfile (_nh_sync_brewfile_path)
    set -l tmpfile (mktemp)

    for line in (cat $brewfile)
        if not string match -qr $pattern $line
            echo $line >> $tmpfile
        end
    end

    mv $tmpfile $brewfile
end

# --- Brew formulae sync ---

function _nh_sync_brews
    set -l mode $argv[1]
    set -l brewfile (_nh_sync_brewfile_path)

    set -l installed (brew leaves 2>/dev/null)
    set -l tracked (string match -r '^brew "(.+)"' < $brewfile | string match -v '^brew')

    if test $mode = keep
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
    set -l tracked (string match -r '^cask "(.+)"' < $brewfile | string match -v '^cask' | string replace -r '.+/' '')

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
