# Add or update a name=path entry in the nh project registry.

function _nh_project_register -a name path
    if test -z "$name"; or test -z "$path"
        echo "_nh_project_register: name and path required" >&2
        return 2
    end

    _nh_ensure_state_dir
    set -l registry (_nh_project_registry_file)
    set -l abs_path (realpath $path 2>/dev/null; or echo $path)

    if not test -f $registry
        echo "$name=$abs_path" > $registry
        return 0
    end

    if grep -q "^$name=" $registry 2>/dev/null
        sed -i '' "s|^$name=.*|$name=$abs_path|" $registry
    else
        echo "$name=$abs_path" >> $registry
    end
end
