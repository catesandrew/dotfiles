# 
# Copyright (c) 2004, Lukas Westermann <contact AT openos DOT ch>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without 
# modification, are permitted provided that the following conditions 
# are met:
#   - Redistributions of source code must retain the above copyright 
#     notice, this list of conditions and the following disclaimer.
#   - Redistributions in binary form must reproduce the above 
#     copyright notice, this list of conditions and the following 
#     disclaimer in the documentation and/or other materials 
#     provided with the distribution.
#   - The names of its contributors may not be used to endorse or 
#     promote products derived from this software without specific 
#     prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND 
# CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF 
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR 
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
# NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, 
# EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

#
# Bash Auto-Completion for the NextStep command 'open'
#
# History
# ----------------------------------
# Version 1.0.0 - initial release
# Version 1.0.1 - added test for availabilty (Darwin only
#                 and check for open)
# Version 1.0.2 - fixed problems with IFS for -a and -e
# Version 1.0.3 - simplified availability check
# Version 1.0.4 - fixed problem with spaces and completion
#

_list_apps() {
    local IFS=$'\n'
    find '/Applications' "${HOME}"'/Applications' '/Developer/Applications' -name "${1}*.app" -maxdepth 2 2>/dev/null
    #TODO: MS Word and other carbon apps...HOW ???
}

_open()
{

    local cur prev IFS=$'\t\n'
    COMPREPLY=()
    cur=${COMP_WORDS[COMP_CWORD]}
    prev=${COMP_WORDS[COMP_CWORD-1]}

    if [[ $prev == -a ]]; then
        local res ncur
        ncur=$( echo "$cur" | sed -e 's/\\//g' 2>/dev/null )
        res=$( _list_apps "${ncur}" | sed -e 's/.*\/\([^\/]*\)\.app/\1/' )
        COMPREPLY=( $( compgen -W "${res}" -- "${ncur}" ) )
    elif [[ $prev == -e ]]; then
        _filedir '@(txt|doc|rtf|rtfd|pdf|png|jpg|gif)'
    elif [[ $cur == -* ]]; then
        local IFS=$' \t\n'
        COMPREPLY=( $( compgen -W '-a -e' -- "$cur" ) )
    else
        local first="${COMP_WORDS[1]}" second="${COMP_WORDS[2]}"
        if [[ $first == -a ]]; then
            case "$second" in
                QuickTime*)
                    _filedir '@(mp3|mpg|mpeg|avi|m4*|mov|alc)'
                    ;;
                iTunes)
                    _filedir '@(mp3|m4*|ogg|alc)'
                    ;;
                Safari|Mozilla|Camino|Opera|Firebird)
                    # TODO: complete from history/bookmarks ?
                    _filedir '@(htm|html|xhtm|xhtml|php|php3|php3|xml)'
                    ;;
                VLC)
                    # TODO: complete for CD/DVD too...
                    _filedir '@(avi|mpg)'
                    ;;
                BitTorrent*)
                    _filedir '@(torrent)'
                    ;;
                *)
                    # others, complete all
                    _filedir
                    ;;
            esac
        else
            _filedir
        fi
    fi

}
complete $filenames -F _open open
