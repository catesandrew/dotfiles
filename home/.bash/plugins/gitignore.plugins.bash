function gi() (
    gi_args=()
    for arg; do
        if [[ $arg = -- ]]; then
            curl_args=("${gi_args[@]}")
            gi_args=()
        else
            gi_args+=("$arg")
        fi
    done
    IFS=,
    curl "${curl_args[@]}" https://www.toptal.com/developers/gitignore/api/"${gi_args[*]}"
)
