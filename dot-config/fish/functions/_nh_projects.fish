# nh projects subcommand
#
# Lists projects under ~/Projects/<org>/<name>.
#
#   nh projects              List all as <org>/<name>
#   nh projects <org>        List projects under that org as <name>
#   nh projects --paths      Include full filesystem paths

function _nh_projects
    set -l show_paths 0
    set -l org ""

    for arg in $argv
        switch $arg
            case --help -h
                echo "Usage: nh projects [<org>] [--paths]"
                echo ""
                echo "List projects under ~/Projects/<org>/<name>."
                echo ""
                echo "Arguments:"
                echo "  <org>      Scope to a single org/user directory"
                echo ""
                echo "Options:"
                echo "  --paths    Include full filesystem paths"
                echo "  --help     Show this help message"
                return 0
            case --paths
                set show_paths 1
            case '-*'
                echo "nh projects: unknown option '$arg'" >&2
                return 1
            case '*'
                if test -n "$org"
                    echo "nh projects: too many arguments (got '$arg' after '$org')" >&2
                    return 1
                end
                set org $arg
        end
    end

    set -l projects_root "$HOME/Projects"

    if not test -d $projects_root
        echo "Projects root not found: $projects_root" >&2
        return 1
    end

    if test -n "$org"
        set -l org_dir "$projects_root/$org"
        if not test -d $org_dir
            echo "Org '$org' not found at $org_dir" >&2
            return 1
        end

        for proj in $org_dir/*/
            set -l name (basename $proj)
            if test $show_paths -eq 1
                echo "$name"\t(string trim --chars=/ -r $proj)
            else
                echo $name
            end
        end | sort
        return 0
    end

    for org_dir in $projects_root/*/
        set -l org_name (basename $org_dir)
        for proj in $org_dir/*/
            set -l proj_name (basename $proj)
            if test $show_paths -eq 1
                echo "$org_name/$proj_name"\t(string trim --chars=/ -r $proj)
            else
                echo "$org_name/$proj_name"
            end
        end
    end | sort
end
