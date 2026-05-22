# Resolve a project name to a filesystem path.
#
# Resolution order for a bare name:
#   1. Registry file (~/.local/state/nh/projects)
#   2. ~/Projects/$name           (direct child, e.g. an org dir is matched literally)
#   3. ~/Projects/*/$name         (one level deep, matching ~/Projects/<org>/<name>)
#
# Accepts "org/name" syntax for explicit targeting (skips ambiguity checks).

function _nh_project_resolve -a name
    if test -z "$name"
        echo "_nh_project_resolve: name required" >&2
        return 2
    end

    # Explicit "org/name" form: just check the path.
    if string match -q '*/*' -- $name
        set -l explicit "$HOME/Projects/$name"
        if test -d $explicit
            echo $explicit
            return 0
        end
        echo "Project '$name' not found at $explicit" >&2
        return 1
    end

    # Registry lookup.
    set -l registry (_nh_project_registry_file)
    if test -f $registry
        set -l hit (grep "^$name=" $registry | head -n1 | string replace -r '^[^=]+=' '')
        if test -n "$hit"; and test -d "$hit"
            echo $hit
            return 0
        end
    end

    # Direct child of ~/Projects (usually an org dir, but supports flat layouts).
    set -l direct "$HOME/Projects/$name"
    if test -d $direct
        echo $direct
        return 0
    end

    # One level deep: ~/Projects/<org>/<name>.
    set -l found
    for org in $HOME/Projects/*/
        set -l candidate "$org$name"
        if test -d $candidate
            set -a found $candidate
        end
    end

    if test (count $found) -eq 1
        echo $found[1]
        return 0
    else if test (count $found) -gt 1
        echo "Ambiguous project '$name'. Matches:" >&2
        for f in $found
            echo "  $f" >&2
        end
        echo "Disambiguate with org/name syntax (e.g. nhessler/$name)." >&2
        return 1
    end

    echo "Could not resolve project '$name'. Tried registry, ~/Projects/$name, ~/Projects/*/$name." >&2
    return 1
end
