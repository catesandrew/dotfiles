#have wm && {
_jira()
{
    local cur prev

    COMPREPLY=()
    _get_comp_words_by_ref cur prev

    case $prev in
        jira)
            COMPREPLY=( $( compgen -W 'help title html2md' -- "$cur" ) )
            return 0
            ;;
    esac
    #echo "Cur: $cur, Prev: $prev"
    if [[ "$cur" == -* ]]; then
        # relevant options completion
        COMPREPLY=( $( compgen -W 'help title html2md' -- "$cur" ) )
    fi

}
complete -F _jira jira
#}


