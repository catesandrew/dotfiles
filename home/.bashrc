if [ $(uname) = "Darwin" ]; then

    export CHEATCOLORS=true
    export JAVA_HOME="`/usr/libexec/java_home`"
    export HTML_TIDY=$HOME/.tidyrc
    export TLSOURCE=en TLTARGET=es
    export LC_CTYPE="utf-8"
    export LC_ALL=en_US.UTF-8
    export LANG=en_US.UTF-8
    export HOMEBREW_CASK_OPTS="--appdir=/Applications --caskroom=/usr/local/caskroom"

    export PERL_LOCAL_LIB_ROOT="${HOME}/perl5:$PERL_LOCAL_LIB_ROOT";
    export PERL_MB_OPT="--install_base "${HOME}/perl5"";
    export PERL_MM_OPT="INSTALL_BASE=${HOME}/perl5";
    export PERL5LIB="${HOME}/perl5/lib/perl5:$PERL5LIB";
    export PATH="/usr/local/bin:/usr/local/sbin:${PATH}"
    export PATH="${HOME}/perl5/bin:${PATH}"
    export PATH="${HOME}/.rbenv/shims:${PATH}"
    export BREW_HOME="`brew --prefix`"
    export BREW_EMACS_MAC_HOME="`brew --prefix emacs-mac`"
    export VAGRANT_VMWARE_CLONE_DIRECTORY="${HOME}/.cache/vagrant"
    export GOPATH=/usr/local/go

elif [ $(uname) = "Linux" ]; then

    export PATH="/usr/local/bin:/usr/local/sbin:$PATH"
    export PATH="/usr/local/linuxbrew/bin:$PATH"
    export BREW_HOME="`brew --prefix`"
    export HOMEBREW_TEMP="${BREW_HOME}/tmp"
    export MANPATH="${BREW_HOME}/share/man:$MANPATH"
    export INFOPATH="${BREW_HOME}/share/info:$INFOPATH"

    # Until LinuxBrew is fixed, the following is required.
    # See: https://github.com/Homebrew/linuxbrew/issues/47
    export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/local/lib64/pkgconfig:/usr/lib64/pkgconfig:/usr/lib/pkgconfig:/usr/lib/x86_64-linux-gnu/pkgconfig:/usr/lib64/pkgconfig:/usr/share/pkgconfig:$PKG_CONFIG_PATH
    ## Setup linux brew
    export PKG_CONFIG_PATH=${BREW_HOME}/lib64/pkgconfig:${BREW_HOME}/lib/pkgconfig:$PKG_CONFIG_PATH
    export LD_LIBRARY_PATH=${BREW_HOME}/lib64:${BREW_HOME}/lib:$LD_LIBRARY_PATH

fi

export PATH="${HOME}/.cabal/bin:$PATH"
export PATH="${HOME}/.bin:$PATH"
export PACKER_CACHE_DIR="${HOME}/.packer_cache"
export DFM_REPO="${HOME}/.dotfiles"

# vim and gnome-terminal have support for 256 colours in fedora 8 at least
# Note debian/ubuntu users need to install the ncurses-term package for this
# Note this should be set in ~/.profile for Fedora startup scripts to
# setup LS_COLORS correctly.
if [[ $COLORTERM = gnome-* && $TERM = xterm ]]  && infocmp gnome-256color >/dev/null 2>&1; then
    export TERM=gnome-256color
elif infocmp xterm-256color >/dev/null 2>&1; then
    export TERM=xterm-256color
fi

# General Startup #
umask 002
mesg n

# Let me have core dumps
#ulimit -c unlimited
ulimit -n 2048

# Autocorrect typos in path names when using `cd`
shopt -s cdspell

# Check the window size after each command and, if necessary, update the values
# of LINES and COLUMNS.
shopt -s checkwinsize

# Append to the Bash history file, rather than overwriting it
shopt -s histappend

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob

# Don't clear the screen after quitting a manual page
export MANPAGER="less -X"

# Change this to your console based IRC client of choice.
export IRC_CLIENT='irssi'

# Node Version Manager
export NVM_TARGET="$(brew --prefix nvm)"
export NVM_DIR="/usr/local/nvm"
[[ -s "$NVM_TARGET/nvm.sh" ]] && source "$NVM_TARGET/nvm.sh" # Load NVM into a shell session *as a function*

# Load composure first, so we support function metadata
source "${BASH_IT}/lib/composure.sh"

# support 'plumbing' metadata
cite _about _param _example _group _author _version

# Load colors first so they can be use in base theme
source "${BASH_IT}/themes/colors.theme.bash"
source "${BASH_IT}/themes/base.theme.bash"

# library
LIB="${BASH_IT}/lib/*.bash"
for config_file in $LIB
do
  source $config_file
done

# Load enabled aliases, completion, plugins
for file_type in "aliases" "completion" "plugins"
do
  _load_bash_it_files $file_type
done

# Load custom aliases, completion, plugins
for file_type in "aliases" "completion" "plugins"
do
  if [ -e "${BASH_IT}/${file_type}/custom.${file_type}.bash" ]
  then
    source "${BASH_IT}/${file_type}/custom.${file_type}.bash"
  fi
done

unset config_file
if [[ $PROMPT ]]; then
    export PS1=$PROMPT
fi

# Adding Support for other OSes
PREVIEW="less"
[ -s /usr/bin/gloobus-preview ] && PREVIEW="gloobus-preview"
[ -s /Applications/Preview.app ] && PREVIEW="/Applications/Preview.app"

# Start Up Scripts #

# fasd
fasd_cache="$HOME/.fasd-init-bash"
if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
  fasd --init posix-alias bash-hook bash-ccomp bash-ccomp-install >| "$fasd_cache"
fi
source "$fasd_cache"
unset fasd_cache

# Setting ag as the default source for fzf
export FZF_DEFAULT_COMMAND='ag -l -g ""'

# If you're running fzf in a large git repository, git ls-tree can boost up the speed of the traversal.
export FZF_DEFAULT_COMMAND='
  (git ls-tree -r --name-only HEAD ||
   find * -name ".*" -prune -o -type f -print -o -type l -print) 2> /dev/null'

# Custom (Machine-Specific) Startup #
. ${BASH_IT}/custom.sh


if [ $(uname) = "Darwin" ]; then
    launchctl setenv PATH $PATH
fi
