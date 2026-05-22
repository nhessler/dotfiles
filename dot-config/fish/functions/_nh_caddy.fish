# nh caddy subcommand
#
# Manages per-project Caddyfile.local files and their import lines in the
# master Caddyfile.
#
#   nh caddy new [project] [--domain D] [--port P] [--static]
#   nh caddy remove [project] [--purge]
#   nh caddy list [--paths]
#   nh caddy reload
#
# The master Caddyfile defaults to ~/Projects/nhessler/dotfiles/caddy/Caddyfile
# and can be overridden via $NH_CADDY_FILE.

function _nh_caddy_master
    if set -q NH_CADDY_FILE
        echo $NH_CADDY_FILE
    else
        echo "$HOME/Projects/nhessler/dotfiles/caddy/Caddyfile"
    end
end

function _nh_caddy_help
    echo "Usage: nh caddy <command> [options]"
    echo ""
    echo "Manage per-project Caddyfile.local files."
    echo ""
    echo "Commands:"
    echo "  new [project]       Create Caddyfile.local and add import to master"
    echo "  remove [project]    Remove import from master (use --purge to also delete the file)"
    echo "  list                List projects currently imported into the master Caddyfile"
    echo "  reload              Gracefully reload the running Caddy server"
    echo "  help                Show this help message"
    echo ""
    echo "Options for 'new':"
    echo "  --domain <host>     Domain to serve (default: <project>.test)"
    echo "  --port <port>       Generate a reverse_proxy to localhost:<port>"
    echo "  --static            Generate a file_server rooted at the project dir"
    echo ""
    echo "Options for 'remove':"
    echo "  --purge             Also delete the project's Caddyfile.local"
    echo ""
    echo "Options for 'list':"
    echo "  --paths             Include the Caddyfile.local path for each entry"
    echo ""
    echo "Master Caddyfile: "(_nh_caddy_master)
    echo "Override with NH_CADDY_FILE environment variable."
end

function _nh_caddy
    set -l subcmd $argv[1]
    set -l args $argv[2..-1]

    switch "$subcmd"
        case new
            _nh_caddy_new $args
        case remove rm
            _nh_caddy_remove $args
        case list ls
            _nh_caddy_list $args
        case reload
            _nh_caddy_reload $args
        case help -h --help ''
            _nh_caddy_help
        case '*'
            echo "nh caddy: unknown command '$subcmd'" >&2
            echo ""
            _nh_caddy_help
            return 1
    end
end

# Resolve "project arg or cwd" → (name, path). Echoes "name<TAB>path".
function _nh_caddy_target -a project_arg
    if test -z "$project_arg"
        set -l path (pwd)
        echo (basename $path)\t$path
        return 0
    end

    set -l resolved (_nh_project_resolve $project_arg)
    if test $status -ne 0
        return 1
    end

    # Project name for display is the last path segment.
    echo (basename $resolved)\t$resolved
end

function _nh_caddy_new
    set -l project ""
    set -l domain ""
    set -l port ""
    set -l static_mode 0

    set -l i 1
    while test $i -le (count $argv)
        set -l arg $argv[$i]
        switch $arg
            case --help -h
                _nh_caddy_help
                return 0
            case --domain
                set i (math $i + 1)
                set domain $argv[$i]
            case --port
                set i (math $i + 1)
                set port $argv[$i]
            case --static
                set static_mode 1
            case '-*'
                echo "nh caddy new: unknown option '$arg'" >&2
                return 1
            case '*'
                if test -n "$project"
                    echo "nh caddy new: too many arguments" >&2
                    return 1
                end
                set project $arg
        end
        set i (math $i + 1)
    end

    set -l target (_nh_caddy_target $project)
    if test $status -ne 0
        return 1
    end

    set -l name (echo $target | cut -f1)
    set -l path (echo $target | cut -f2)

    if test -z "$domain"
        set domain "$name.test"
    end

    set -l local_file "$path/Caddyfile.local"
    if test -e $local_file
        echo "Caddyfile.local already exists at $local_file" >&2
        echo "Edit it directly, or run: nh caddy remove $name --purge" >&2
        return 1
    end

    # Generate template.
    set -l template
    if test -n "$port"
        set template "$domain {
    tls internal
    reverse_proxy localhost:$port
}
"
    else if test $static_mode -eq 1
        set template "$domain {
    tls internal
    root * $path
    file_server
}
"
    else
        set template "# Fill in your site config. Examples:
#
# $domain {
#     tls internal
#     reverse_proxy localhost:PORT
# }
#
# $domain {
#     tls internal
#     root * $path
#     file_server
# }
"
    end

    printf '%s' "$template" > $local_file
    echo "Created $local_file"

    # Append import to master, unless already present.
    set -l master (_nh_caddy_master)
    if not test -f $master
        echo "Master Caddyfile not found: $master" >&2
        echo "Set NH_CADDY_FILE or create the file, then re-run." >&2
        return 1
    end

    set -l import_line "import $local_file"
    if grep -qxF -- $import_line $master 2>/dev/null
        echo "Import already present in $master (no change)"
    else
        echo $import_line >> $master
        echo "Appended import to $master"
    end

    _nh_project_register $name $path

    echo ""
    echo "Next: edit $local_file, then run: nh caddy reload"
end

function _nh_caddy_remove
    set -l project ""
    set -l purge 0

    for arg in $argv
        switch $arg
            case --help -h
                _nh_caddy_help
                return 0
            case --purge
                set purge 1
            case '-*'
                echo "nh caddy remove: unknown option '$arg'" >&2
                return 1
            case '*'
                if test -n "$project"
                    echo "nh caddy remove: too many arguments" >&2
                    return 1
                end
                set project $arg
        end
    end

    set -l target (_nh_caddy_target $project)
    if test $status -ne 0
        return 1
    end

    set -l name (echo $target | cut -f1)
    set -l path (echo $target | cut -f2)
    set -l local_file "$path/Caddyfile.local"
    set -l master (_nh_caddy_master)

    if not test -f $master
        echo "Master Caddyfile not found: $master" >&2
        return 1
    end

    set -l import_line "import $local_file"
    if grep -qxF -- $import_line $master 2>/dev/null
        set -l tmp (mktemp)
        grep -vxF -- $import_line $master > $tmp
        mv $tmp $master
        echo "Removed import from $master"
    else
        echo "No import for $local_file found in $master"
    end

    if test $purge -eq 1
        if test -f $local_file
            rm $local_file
            echo "Deleted $local_file"
        end
        _nh_project_unregister $name
    end

    echo ""
    echo "Next: run: nh caddy reload"
end

function _nh_caddy_list
    set -l show_paths 0

    for arg in $argv
        switch $arg
            case --help -h
                _nh_caddy_help
                return 0
            case --paths
                set show_paths 1
            case '-*'
                echo "nh caddy list: unknown option '$arg'" >&2
                return 1
        end
    end

    set -l master (_nh_caddy_master)
    if not test -f $master
        echo "Master Caddyfile not found: $master" >&2
        return 1
    end

    # Imports look like: `import /abs/path/Caddyfile.local`
    set -l imports (grep -E '^import [^[:space:]]+/Caddyfile\.local$' $master 2>/dev/null | string replace -r '^import ' '')

    if test (count $imports) -eq 0
        echo "No projects imported in $master"
        return 0
    end

    for import in $imports
        set -l project_name (basename (dirname $import))

        if not test -f $import
            echo "$project_name: (missing) — $import"
            continue
        end

        # Site-block opener: starts with non-whitespace non-comment, ends with `{`.
        set -l domains
        for line in (grep -E '^[^[:space:]#].*\{[[:space:]]*$' $import)
            set -l head (string replace -r '\s*\{\s*$' '' -- $line | string trim)
            for d in (string split ',' -- $head)
                set -a domains (string trim $d)
            end
        end

        set -l info "(no domains)"
        if test (count $domains) -gt 0
            set info (string join ', ' $domains)
        end

        if test $show_paths -eq 1
            echo "$project_name: $info  →  $import"
        else
            echo "$project_name: $info"
        end
    end
end

function _nh_caddy_reload
    set -l master (_nh_caddy_master)

    if not command -q caddy
        echo "caddy not installed" >&2
        return 1
    end

    if not test -f $master
        echo "Master Caddyfile not found: $master" >&2
        return 1
    end

    caddy reload --config $master
end
