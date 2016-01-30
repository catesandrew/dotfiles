#have wm && {
_wm_projects()
{
    COMPREPLY=( $( compgen -W 'phoenix thorax \
        all' -- "$cur" ) )
}

_wm_lumbar()
{
    COMPREPLY=( $( compgen -W 'kill home base checkout \
        all' -- "$cur" ) )
}

_wm()
{
    local cur prev

    COMPREPLY=()
    _get_comp_words_by_ref cur prev

    case $prev in
        start-track|stop-track|format-patch|apply-patch|discard|rebase|log)
            _wm_projects
            return 0
            ;;
        lumbar)
            _wm_lumbar
            return 0
            ;;
        wm)
            COMPREPLY=( $( compgen -W 'help run stop kill status \
                start-track stop-track format-patch apply-patch \
                discard lumbar rebase log' -- "$cur" ) )
            return 0
            ;;
    esac
    #echo "Cur: $cur, Prev: $prev"
    if [[ "$cur" == -* ]]; then
        # relevant options completion
        COMPREPLY=( $( compgen -W 'help run stop kill status \
            start-track stop-track format-patch apply-patch \
            discard lumbar rebase log' -- "$cur" ) )
    fi

}
complete -F _wm wm
#}


