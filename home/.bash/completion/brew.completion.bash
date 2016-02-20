#!/bin/bash

if [ "$__dot_system_type" == "Darwin" ]; then
    if brew_contains_element "bash-completion"; then
        BASH_COMP_TARGET="$__dot_brew_home/etc/bash_completion"
        if [ "${BASH_VERSINFO[0]}" -le 3 ] && [ -f "${BASH_COMP_TARGET}" ]; then
          . "${BASH_COMP_TARGET}"
        fi
        unset BASH_COMP_TARGET
    elif brew_contains_element "bash-completion2"; then
        BASH_COMP_TARGET="$__dot_brew_home/share/bash-completion/bash_completion"

        if [ "${BASH_VERSINFO[0]}" -ge 4 ] && [ -f "${BASH_COMP_TARGET}" ]; then
          . "${BASH_COMP_TARGET}"
        fi
        unset BASH_COMP_TARGET
    fi
elif [ "$__dot_system_type" == "Linux" ]; then
    if ! shopt -oq posix; then
        if [ -f /usr/share/bash-completion/bash_completion ]; then
            . /usr/share/bash-completion/bash_completion
        elif [ -f /etc/bash_completion ]; then
            . /etc/bash_completion
        fi
    fi
fi
