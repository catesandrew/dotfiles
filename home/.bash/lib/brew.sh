#!/bin/bash

# Test whether a Homebrew formula is already installed
# $1 - formula name (may include options)
formula_exists() {
    if $(brew list $1 >/dev/null); then
        printf "%s already installed.\n" "$1"
        return 0
    fi

    e_warning "Missing formula: $1"
    return 1
}

# Test whether a command exists
# $1 - cmd to test
type_exists() {
    if [ $(type -P $1) ]; then
      return 0
    fi
    return 1
}


run_brew() {
    # Check for Homebrew
    if type_exists 'brew'; then
        e_header "Updating Homebrew..."
        # Use the latest version of Homebrew
        brew update
        [[ $? ]] && e_success "Done"

        e_header "Updating any existing Homebrew formulae..."
        # Upgrade any already-installed formulae
        brew upgrade
        [[ $? ]] && e_success "Done"

        e_header "Checking status of desired Homebrew formulae..."
        local list_formulae
        local -a missing_formulae
        local -a desired_formulae=(
            'coreutils' # GNU core utilities (those that come with OS X are outdated)
            'git'
            'ack'
            'bash'
            'bash-completion'
            'ffmpeg'
            'graphicsmagick'
            'jpeg'
            'macvim --override-system-vim'
            'node'
            'optipng'
            'phantomjs'
            'tree'
            'wget'
        )

        for index in ${!desired_formulae[*]}
        do
            if ! formula_exists ${desired_formulae[$index]}; then
                # Store the name (and options) of every missing formula
                missing_formulae=("${missing_formulae[@]}" "${desired_formulae[$index]}")
            fi
        done

        if [[ "$missing_formulae" ]]; then
            # Convert the array of missing formula into a list of space-separate strings
            list_formulae=$( printf "%s " "${missing_formulae[@]}" )

            e_header "Installing missing Homebrew formulae..."
            # Install all missing formulae
            brew install $list_formulae

            [[ $? ]] && e_success "Done"
        fi

        # use latest rsync rather than out-dated OS X rsync
        # install separately from the main formulae list to fix gh-19
        brew install https://raw.github.com/Homebrew/homebrew-dupes/master/rsync.rb

        # Remove outdated versions from the Cellar
        brew cleanup
    else
        printf "\n"
        e_error "Error: Homebrew not found."
        printf "Aborting...\n"
        exit
    fi

}
