# xcodebuild
# Bash command line completion for xcodebuild
#
# Created by Jonathon Mah on 2006-11-08.
# Copyright 2006 Playhaus. All rights reserved.
#
# This file is licensed under the BSD license, as follows:
#
# Copyright (c) 2006, Playhaus
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the name of the Playhaus nor the names of its contributors may be
#   used to endorse or promote products derived from this software without
#   specific prior written permission.
#
# This software is provided by the copyright holders and contributors "as is"
# and any express or implied warranties, including, but not limited to, the
# implied warranties of merchantability and fitness for a particular purpose are
# disclaimed. In no event shall the copyright owner or contributors be liable
# for any direct, indirect, incidental, special, exemplary, or consequential
# damages (including, but not limited to, procurement of substitute goods or
# services; loss of use, data, or profits; or business interruption) however
# caused and on any theory of liability, whether in contract, strict liability,
# or tort (including negligence or otherwise) arising in any way out of the use
# of this software, even if advised of the possibility of such damage.
#
# Version 1.0 (2006-11-08)

_xcodebuild()
{
    local cur prev #cmds cmdOpts optsParam opt helpCmds optBase i

    COMPREPLY=()
    cur=${COMP_WORDS[COMP_CWORD]}
    prev=${COMP_WORDS[COMP_CWORD-1]}

    cmds='-project -activetarget -alltargets -target -activeconfiguration -configuration -version -list -help build installsrc install clean'

    if [[ "$prev" = "-project" ]]; then
        # Show list of projects
        local projects=$( compgen -X "${1:+"!*.xcodeproj"}" -d | sed -e 's/ /\\ /g' )
        local IFS=$'\n'
        COMPREPLY=( $( echo "$projects" | grep "^$cur" ) )
    elif [[ "$prev" = "-target" ]]; then
        local arg project_args is_project_next
        is_project_next="false"
        for arg in ${COMP_WORDS} ; do
            if [[ "$is_project_next" = "true" ]]; then
                project_args="-project $arg"
                break
            elif [[ "$arg" = "-project" ]]; then
                is_project_next="true"
            fi
        done
        cur=$(printf '%q' "$cur" )
        local targets=$(xcodebuild $project_args -list | sed -n -e '/^    Targets:$/,/^$/p' | sed -e '1d' -e '$d' -e 's/^[[:space:]]*//' -e 's/ (Active)$//' -e 's/ /\\ /g' | grep "^$cur" )
        local IFS=$'\n'
        COMPREPLY=( $targets )
    elif [[ "$prev" = "-configuration" ]]; then
        local arg project_args is_project_next
        is_project_next="false"
        for arg in ${COMP_WORDS} ; do
            if [[ "$is_project_next" = "true" ]]; then
                project_args="-project $arg"
                break
            elif [[ "$arg" = "-project" ]]; then
                is_project_next="true"
            fi
        done
        cur=$(printf '%q' "$cur" )
        local configurations=$(xcodebuild $project_args -list | sed -n -e '/^    Build Configurations:$/,/^$/p' | sed -e '1d' -e '$d' -e 's/^[[:space:]]*//' -e 's/ (Active)$//' -e 's/ /\\ /g' | grep "^$cur" )
        local IFS=$'\n'
        COMPREPLY=( $configurations )
    else
        COMPREPLY=( $( compgen -W "$cmds" -- $cur ) )
    fi

    return 0
}
complete -F _xcodebuild xcodebuild
