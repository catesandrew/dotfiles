# OSX's hdiutil completion                                  -*- shell-script -*-
#
# Copyright (c) 2015 [Adam Strzelecki](http://www.nanoant.com)
#
# Distributed under the [MIT License](http://creativecommons.org/licenses/MIT/)

if [ "$__dot_system_type" == "Darwin" ] && [ "${BASH_VERSINFO[0]}" -ge 4 ]; then
_hdiutil()
{
        local cur prev words cword split
        _init_completion -s || return

        if [[ $cword -eq 1 ]]; then
                COMPREPLY=( $( compgen -W '\
                help      flatten\
                attach    imageinfo\
                detach    internet-enable\
                eject     isencrypted\
                verify    makehybrid\
                create    mount\
                compact   mountvol\
                convert   unmount\
                burn      plugins\
                info      resize\
                checksum  segment\
                chpass    pmap\
                erasekeys udifderez\
                unflatten udifrez' -- "$cur" ) )
                return
        fi

        if [[ $cword -gt 2 && "$prev" == -* ]]; then
                case $prev in
                -fs)
                        local oifs="$IFS"
                        IFS=$'\n' COMPREPLY=( $( IFS=: compgen -W 'ExFAT:Case-sensitive Journaled HFS+:HFS+:Journaled HFS+:Case-sensitive HFS+:MS-DOS:MS-DOS FAT12:MS-DOS FAT32:MS-DOS FAT16:UDF' -- "$cur" ) )
                        IFS="$oifs"
                        compopt -o filenames
                        return ;;
                -type)
                        COMPREPLY=( $( compgen -W 'SPARSEBUNDLE SPARSE UDIF UDTO' -- "$cur" ) )
                        return ;;
                -format)
                        COMPREPLY=( $( compgen -W 'DC42 IPOD RdWr Rdxx Rken ROCo SPARSE SPARSEBUNDLE UDBZ UDCO UDIF UDRO UDRW UDSB UDSP UDTO UDxx UDZO UFBI UNIV' -- "$cur" ) )
                        return ;;
                -encryption)
                        COMPREPLY=( $( compgen -W 'AES-128 AES-256' -- "$cur" ) )
                        return ;;
                -size|-stretch|-segmentSize)
                        if [[ $cur == *[bkmgtpe] ]]; then
                                COMPREPLY=( $( compgen -W "$cur" ) )
                        elif [[ $cur ]]; then
                                COMPREPLY=( $( compgen -P "$cur" -W "b k m g t p e" ) )
                                compopt -o nospace
                        else
                                COMPREPLY=( $( compgen -W "{0..9}" ) )
                                compopt -o nospace
                        fi
                        return ;;
                esac
        fi

        if [[ "$cur" == -* ]]; then
                COMPREPLY=( $( compgen -W '$( _parse_help "$1" "${words[1]} -help" ) -help' -- "$cur" ) )
        fi

        _filedir
        return 0

} &&
complete -F _hdiutil hdiutil
fi

# ex: ts=4 sw=4 et filetype=sh
