# Remove a name's entry from the nh project registry.

function _nh_project_unregister -a name
    if test -z "$name"
        echo "_nh_project_unregister: name required" >&2
        return 2
    end

    set -l registry (_nh_project_registry_file)
    if not test -f $registry
        return 0
    end

    sed -i '' "/^$name=/d" $registry
end
