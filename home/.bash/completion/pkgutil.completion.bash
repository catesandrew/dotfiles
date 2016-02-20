# OSX's pkgutil completion                                  -*- shell-script -*-
#
# Copyright (c) 2015 [Adam Strzelecki](http://www.nanoant.com)
#
# Distributed under the [MIT License](http://creativecommons.org/licenses/MIT/)

if [ "$__dot_system_type" == "Darwin" ] && [ "${BASH_VERSINFO[0]}" -ge 4 ]; then
_pkgutil()
{
        local cur prev words cword split
        _init_completion -s || return

        case $prev in
        --bom|\
        --expand|\
        --file-info-plist|\
        --file-info|\
        --payload-files)
                _filedir
                return 0
                ;;
        --flatten|\
        --volume)
                _filedir -d
                return 0
                ;;
        --edit-pkg|\
        --export-plist|\
        --files|\
        --forget|\
        --info|\
        --lsbom|\
        --pkg-groups|\
        --pkg-info-plist|\
        --pkg-info|\
        --repair|\
        --verify)
                COMPREPLY=( $( compgen -W '$( pkgutil --pkgs )' -- "$cur" ) )
                return 0
                ;;
        esac

        # 2nd argument
        if [[ $cword -gt 2 ]]; then
                case ${words[$cword-2]} in
                --expand)  _filedir -d; return 0 ;;
                --flatten) _filedir;    return 0 ;;
                esac
        fi

        $split && return 0

        if [[ "$cur" == -* ]]; then
                COMPREPLY=( $( compgen -W '$( _parse_help "$1" ) --info' -- "$cur" ) )
                return 0
        fi
} &&
complete -F _pkgutil pkgutil
fi

# ex: ts=4 sw=4 et filetype=sh
