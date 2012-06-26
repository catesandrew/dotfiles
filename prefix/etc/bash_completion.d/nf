#have nf && {
_nf_branches()
{
    COMPREPLY=( $( compgen -W 'ecweb ecadmin account-web \
        common root baseline monthly release all' -- "$cur" ) )
}

_nf()
{
    local cur prev

    COMPREPLY=()
    _get_comp_words_by_ref cur prev

    case $prev in
        run|stop|start-track|stop-track|format-patch|apply-patch|discard|dux|rebase|log)
            _nf_branches
            return 0
            ;;
        nf)
            COMPREPLY=( $( compgen -W 'help run stop kill status \
                start-track stop-track format-patch apply-patch \
                discard dux rebase log' -- "$cur" ) )
            return 0
            ;;
    esac
#echo "Cur: $cur, Prev: $prev"
    if [[ "$cur" == -* ]]; then
        # relevant options completion
        COMPREPLY=( $( compgen -W 'help run stop kill status \
              start-track stop-track format-patch apply-patch \
              discard dux rebase log' -- "$cur" ) )
    fi
    
}
complete -F _nf nf
#}


