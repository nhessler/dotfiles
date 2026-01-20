# nh add-keys subcommand

function _nh_add_keys
    set -l dry_run 0

    # Handle flags
    if contains -- --help $argv; or contains -- -h $argv
        echo "Usage: nh add-keys [--dry-run]"
        echo ""
        echo "Add SSH keys to the agent using macOS keychain."
        echo ""
        echo "Options:"
        echo "  --dry-run    Show which keys would be added without adding them"
        echo ""
        echo "This scans ~/.ssh/ for private keys and adds them to the SSH agent"
        echo "using the macOS keychain for passphrase storage."
        echo ""
        echo "Ignore list: ~/.local/state/nh/ssh-keys-ignore"
        echo "  Add one filename per line (just the filename, not full path)"
        echo "  Example: id_old_unused"
        return 0
    end

    if contains -- --dry-run $argv
        set dry_run 1
    end

    set -l ssh_dir "$HOME/.ssh"
    set -l ignore_file (_nh_state_dir)/ssh-keys-ignore
    set -l ignored_names

    # Load ignore list if it exists
    if test -f $ignore_file
        set ignored_names (cat $ignore_file | string trim | string match -v '^#*' | string match -v '^$')
    end

    # Files to always skip
    set -l skip_files config known_hosts known_hosts.old authorized_keys environment agent.env

    set -l keys_found 0
    set -l keys_added 0

    echo "Scanning $ssh_dir for private keys..."
    echo ""

    for file in $ssh_dir/*
        # Skip directories
        if test -d $file
            continue
        end

        set -l filename (basename $file)

        # Skip public keys
        if string match -q '*.pub' $filename
            continue
        end

        # Skip known non-key files
        if contains $filename $skip_files
            continue
        end

        # Check if it's actually a private key by looking at contents
        if not grep -q 'PRIVATE KEY' $file 2>/dev/null
            continue
        end

        set keys_found (math $keys_found + 1)

        # Check ignore list
        if contains $filename $ignored_names
            echo "  Skipping $filename (in ignore list)"
            continue
        end

        if test $dry_run -eq 1
            echo "  Would add: $filename"
        else
            echo "  Adding: $filename"
            ssh-add --apple-use-keychain $file 2>/dev/null
            if test $status -eq 0
                set keys_added (math $keys_added + 1)
            else
                echo "    Failed to add (may need passphrase in keychain first)"
            end
        end
    end

    echo ""
    if test $dry_run -eq 1
        echo "Found $keys_found private key(s). Run without --dry-run to add them."
    else
        echo "Added $keys_added of $keys_found key(s) to agent."
    end
end
