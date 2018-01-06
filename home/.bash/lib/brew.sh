#!/bin/bash

e_header() {
  printf "\n$(tput setaf 7)%s$(tput sgr0)\n" "$@"
}

# Success logging
e_success() {
  printf "$(tput setaf 64)✓ %s$(tput sgr0)\n" "$@"
}

# Error logging
e_error() {
  printf "$(tput setaf 1)x %s$(tput sgr0)\n" "$@"
}

# Warning logging
e_warning() {
  printf "$(tput setaf 136)! %s$(tput sgr0)\n" "$@"
}

contains_element () {
  local f="$1"
  local e match="$1"
  shift
  for e; do [[ "$e" == "$match" ]] && e_success "$e already installed." && return 0; done
  e_warning "Missing formula: $f" && return 1
}

# Test whether a Homebrew formula is already installed
# $1 - formula name (may include options)
formula_exists() {
  if $(brew list "$1" >/dev/null); then
    e_success "$1 already installed."
    return 0
  fi

  e_warning "Missing formula: $1"
  return 1
}

# Test whether a command exists
# $1 - cmd to test
type_exists() {
  if [ "$(type -P "$1")" ]; then
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

    e_header "Updating any existing Homebrew taps..."
    local __brew_taps=($(brew tap | sed 's/:.*//'))

    local -a desired_tap=(
      'caskroom/cask'
      'caskroom/drivers'
      'caskroom/fonts'
      'caskroom/versions'
      'dart-lang/dart'
      'homebrew/boneyard'
      'homebrew/bundle'
      'homebrew/core'
      'homebrew/services'
      'neovim/neovim'
      'thoughtbot/formulae'
      'universal-ctags/universal-ctags'
    )

    for index in ${!desired_tap[*]}; do
      brew tap "${desired_tap[$index]}" >/dev/null
    done

    e_header "Updating any existing Homebrew formulae..."
    # Upgrade any already-installed formulae
    brew upgrade
    [[ $? ]] && e_success "Done"

    # brew cask install 'xquartz'

    e_header "Checking status of desired Homebrew formulae..."
    local __brew_list=($(brew list | sed 's/:.*//'))

    # TODO Build the list so dependencies align properly
    local list_formulae
    local -a missing_formulae
    local -a desired_formulae=(
      'ack'
      'adns'
      'ansible'
      'ansiweather'
      'apparix'
      'archey'
      'asciidoc'
      'asciinema'
      'aspcud'
      'aspell'
      'atk'
      'autoconf'
      'automake'
      'awscli'
      'babel'
      'bash'
      'bash-completion@2'
      'bash-git-prompt'
      'bashish'
      'bcrypt'
      'bdw-gc'
      'berkeley-db'
      'bfg'
      'bibutils'
      'bind'
      'binutils'
      'boost'
      'boost-python'
      'boot-clj'
      'brew-cask-completion'
      'bzip2'
      'bzr'
      'c-ares'
      'cabal-install'
      'cabextract'
      'cadaver'
      'cairo'
      'camlp4'
      'carthage'
      'ccat'
      'cheat'
      'chicken'
      'chrome-cli'
      'chromedriver'
      'clasp'
      'clingo'
      'clisp'
      'cloc'
      'cloog'
      'closure-compiler'
      'closure-stylesheets'
      'cmake'
      'cnats'
      'codequery'
      'colordiff'
      'consul'
      'consul-template'
      'coreutils'
      'cowsay'
      'cscope'
      'csv-fix'
      'cunit'
      'curl'
      'curlish'
      'czmq'
      'dateutils'
      'dbus'
      'dcraw'
      'deisctl'
      'dialog'
      'diction'
      'diff-pdf'
      'diff-so-fancy'
      'diffutils'
      'direnv'
      'dirmngr'
      'django-completion'
      'djvulibre'
      'dnsmasq'
      'docbook'
      'docbook-xsl'
      'docker'
      'docker-clean'
      'docker-cloud'
      'docker-compose'
      'docker-credential-helper'
      'docker-gen'
      'docker-machine'
      'docker-swarm'
      'docutils'
      'dos2unix'
      'doxygen'
      'drip'
      'duti'
      'dwdiff'
      'ecj'
      'editorconfig'
      'elinks'
      'emacs'
      'exif'
      'exiftags'
      'exiftool'
      'expat'
      'expect'
      'faac'
      'faad2'
      'fasd'
      'ffmpeg'
      'fftw'
      'figlet'
      'findutils'
      'fontconfig'
      'fontforge'
      'fortune'
      'fpp'
      'freetype'
      'fswatch'
      'fzf'
      'gawk'
      'gcc'
      'gd'
      'gdbm'
      'gdk-pixbuf'
      'geoip'
      'getdns'
      'gettext'
      'ghc'
      'ghostscript'
      'giflib'
      'gifsicle'
      'gist'
      'git'
      'git-extras'
      'git-flow-avh'
      'git-hooks'
      'git-lfs'
      'git-subrepo'
      'git-tracker'
      'glib'
      'global'
      'gmime'
      'gmp'
      'gnu-getopt'
      'gnu-indent'
      'gnu-sed'
      'gnu-tar'
      'gnu-time'
      'gnu-which'
      'gnupg'
      'gnuplot'
      'gnutls'
      'go'
      'gobject-introspection'
      'gpac'
      'gpg-agent'
      'gradle'
      'graphicsmagick'
      'graphite2'
      'graphviz'
      'grc'
      'grep'
      'gringo'
      'gsasl'
      'gtk+'
      'gts'
      'gzip'
      'hachoir-metadata'
      'handbrake'
      'haproxy'
      'harfbuzz'
      'haskell-stack'
      'hicolor-icon-theme'
      'highlight'
      'html-xml-utils'
      'htmlcleaner'
      'httpie'
      'httrack'
      'hub'
      'hunspell'
      'icu4c'
      'id3tool'
      'ilmbase'
      'imagemagick'
      'intltool'
      'irssi'
      'isl'
      'ldapvi'
      'jansson'
      'jasper'
      'jbig2dec'
      'jemalloc'
      'jo'
      'jpeg'
      'jpeg-archive'
      'jpeg-turbo'
      'jpeginfo'
      'jpegoptim'
      'jq'
      'jshon'
      'jsonlint'
      'jsonpp'
      'juju'
      'known_hosts'
      'kubernetes-cli'
      'lame'
      'languagetool'
      'lastpass-cli'
      'latex2rtf'
      'latexml'
      'launchctl-completion'
      'lcdf-typetools'
      'ledger'
      'leiningen'
      'leptonica'
      'less'
      'lesspipe'
      'libassuan'
      'libatomic_ops'
      'libcaca'
      'libcroco'
      'libdnet'
      'libdvdcss'
      'libev'
      'libevent'
      'libexif'
      'libffi'
      'libgcrypt'
      'libgpg-error'
      'libgphoto2'
      'libgsf'
      'libicns'
      'libiconv'
      'libidn'
      'libiscsi'
      'libksba'
      'liblqr'
      'libmpc'
      'libnfs'
      'libogg'
      'libpng'
      'libpq'
      'libquvi'
      'librevenge'
      'librsvg'
      'libsigsegv'
      'libsodium'
      'libssh'
      'libssh2'
      'libsvg'
      'libsvg-cairo'
      'libtasn1'
      'libtermkey'
      'libtiff'
      'libtool'
      'libunistring'
      'libusb'
      'libusb-compat'
      'libuv'
      'libvo-aacenc'
      'libvorbis'
      'libvpx'
      'libvterm'
      'libwebm'
      'libwmf'
      'libwpd'
      'libxml2'
      'libyaml'
      'libzip'
      'lighttpd'
      'liquidprompt'
      'litmus'
      'little-cms2'
      'llvm'
      'lmdb'
      'logrotate'
      'lua'
      'luajit'
      'lxc'
      'lynx'
      'lzlib'
      'lzo'
      'm4'
      'mackup'
      'macvim'
      'mad'
      'mailutils'
      'make'
      'makedepend'
      'mas'
      'maven'
      'md5sha1sum'
      'mdv'
      'media-info'
      'mercurial'
      'minio-mc'
      'mobile-shell'
      'mosh'
      'mozjpeg'
      'mpfr'
      'mr'
      'msgpack'
      'mtr'
      'multimarkdown'
      'multitail'
      'mupdf-tools'
      'mycli'
      'nasm'
      'ncdu'
      'neofetch'
      'neon'
      'neovim'
      'net-snmp'
      'netcat'
      'netpbm'
      'nettle'
      'nghttp2'
      'nmap'
      'node'
      'node@4'
      'node@6'
      'notmuch'
      'npth'
      'nvm'
      'ocaml'
      'ocamlbuild'
      'oniguruma'
      'opam'
      'open-completion'
      'openconnect'
      'openexr'
      'openjpeg'
      'openssh'
      'openssl'
      'optipng'
      'opus'
      'orc'
      'otto'
      'p11-kit'
      'p7zip'
      'packer'
      'packer-completion'
      'pandoc'
      'pango'
      'par'
      'parallel'
      'pcre'
      'pdfcrack'
      'pdfgrep'
      'pgcli'
      'phantomjs'
      'pigz'
      'pinentry'
      'pinentry-mac'
      'pixman'
      'pkg-config'
      'plantuml'
      'pmd'
      'pngcrush'
      'pngnq'
      'pngquant'
      'poco'
      'poppler'
      'popt'
      'poster'
      'postgresql'
      'potrace'
      'protobuf'
      'pstree'
      'pth'
      'purescript'
      'py2cairo'
      'pyenv'
      'pyenv-virtualenv'
      'pygobject3'
      'pyqt'
      'python'
      'python3'
      'qpdf'
      'qscintilla2'
      'qt'
      'quvi'
      'ranger'
      'rbenv'
      'rcm'
      're2c'
      'readline'
      'reattach-to-user-namespace'
      'recode'
      'redis'
      'rename'
      'renameutils'
      'rethinkdb'
      'roswell'
      'rsync'
      'rtmpdump'
      'ruby'
      'ruby-build'
      'rust'
      'sane-backends'
      'sbcl'
      'scheme48'
      'scons'
      'screenresolution'
      'sdl'
      'sdl_image'
      'selecta'
      'selenium-server-standalone'
      'serf'
      'shared-mime-info'
      'shellcheck'
      'sip'
      'sl'
      'smartypants'
      'spark'
      'spdylay'
      'sphinx-doc'
      'sqlite'
      'sqlmap'
      'ssh-copy-id'
      'sslmate'
      'stoken'
      'stow'
      'surfraw'
      'tag'
      'taglib'
      'talloc'
      'task-spooler'
      'terminal-notifier'
      'tesseract'
      'texi2html'
      'texinfo'
      'the_platinum_searcher'
      'the_silver_searcher'
      'thefuck'
      'theora'
      'tidy-html5'
      'tmux'
      'tmuxinator-completion'
      'tnef'
      'tor'
      'transmission'
      'trash'
      'tree'
      'ttfautohint'
      'tvnamer'
      'txt2tags'
      'typescript'
      'ucl'
      'unbound'
      'unibilium'
      'unison'
      'unrar'
      'unrtf'
      'unzip'
      'upx'
      'urlview'
      'utf8proc'
      'vagrant-completion'
      'vault'
      'vcprompt'
      'w3m'
      'watch'
      'watchman'
      'wdiff --with-gettext'
      'webalizer'
      'webkit2png'
      'webp'
      'wget'
      'wrk'
      'wxmac'
      'x264'
      'x265'
      'xapian'
      'xmlstarlet'
      'xmlto'
      'xvid'
      'xz'
      'yarn'
      'yasm'
      'zeromq'
      'zlib'
      'zopfli'
    )

    # TODO: Log unfound packages so user may delete them

    for index in ${!desired_formulae[*]}; do
      if ! contains_element "${desired_formulae[$index]}" "${__brew_list[@]}"; then
        # Store the name (and options) of every missing formula
        missing_formulae=("${missing_formulae[@]}" "${desired_formulae[$index]}")
      fi
    done

    if [[ "$missing_formulae" ]]; then
      e_header "Installing missing Homebrew formulae..."

      for item in "${missing_formulae[@]}"; do
        e_header "Installing $item..."
        case "$item" in
          emacs)
            brew install emacs --HEAD --with-cocoa --with-gnutls --with-imagemagick@6 --with-librsvg --with-modules;
            ;;
          node)
            brew install node --without-npm
            ;;
          node@4)
            brew install node@4 --without-npm
            ;;
          node@6)
            brew install node@6 --without-npm
            ;;
          *)
            brew install $item
        esac
      done

      # Convert the array of missing formula into a list of space-separate strings
      # list_formulae=$( printf "%s " "${missing_formulae[@]}" )

      # e_header "Installing missing Homebrew formulae..."
      # brew install $list_formulae

      [[ $? ]] && e_success "Done"
    fi

    __cask_list=($(brew cask list | sed 's/:.*//'))
    local cask_list_formulae
    local -a cask_missing_formulae
    local -a cask_desired_formulae=(
      '1password'
      '1password-cli'
      'android-sdk'
      'app-tamer'
      'atom'
      'audio-hijack'
      'bartender'
      'beamer'
      'bettertouchtool'
      'betterzip'
      'betterzipql'
      'bitbar'
      'busycontacts'
      'butler'
      'calibre'
      'charles'
      'cheatsheet'
      'clarify'
      'cloudapp'
      'coderunner'
      'commander-one'
      'controlplane'
      'curio'
      'dash'
      'data-rescue'
      'deltawalker'
      'droplr'
      'eaglefiler'
      'easysimbl'
      'electron'
      'epubmdimporter'
      'epubquicklook'
      'fastlane'
      'firefox'
      'firefoxdeveloperedition'
      'flux'
      'focused'
      'foldingtext-dev'
      'font-open-iconic'
      'font-open-sans'
      'font-raleway'
      'font-xits'
      'fontforge'
      'forklift'
      'gas-mask'
      'genymotion'
      'ghost'
      'gitbook-editor'
      'gitup'
      'google-chrome-canary'
      'hammerspoon'
      'handbrake'
      'hazel'
      'houdahspot'
      'hyper'
      'iconjar'
      'ifunbox'
      'imagealpha'
      'imagemin'
      'imageoptim'
      'istat-menus'
      'iterm2'
      'jaikoz'
      'java'
      'java6'
      'java8'
      'karabiner-elements'
      'keepassx'
      'keyboard-maestro'
      'kindle'
      'kindlegen'
      'launchbar'
      'launchcontrol'
      'leech'
      'licecap'
      'lightpaper'
      'macpass'
      'mactex'
      'mailmate'
      'maltego-casefile'
      'maltego-ce'
      'markdownmdimporter'
      'mattermost'
      'mediainfo'
      'minikube'
      'moom'
      'name-mangler'
      'netspot'
      'obs'
      'omnifocus'
      'omnigraffle'
      'omnioutliner'
      'omniplan'
      'optionspace'
      'pacifist'
      'paw'
      'pdfexpert'
      'pdfkey-pro'
      'pdfpenpro'
      'platypus'
      'prefs-editor'
      'provisionql'
      'psequel'
      'qlcolorcode'
      'qlimagesize'
      'qlmarkdown'
      'qlnetcdf'
      'qlprettypatch'
      'qlrest'
      'qlstephen'
      'qlvideo'
      'querious'
      'quicklook-csv'
      'quicklook-json'
      'quicknfo'
      'quotefix'
      'rcdefaultapp'
      'react-native-debugger'
      'resolutionator'
      'scapple'
      'screenflow'
      'scrivener'
      'selfcontrol'
      'sigil'
      'skim'
      'slack'
      'sonos'
      'spamsieve'
      'suspicious-package'
      'the-escapers-flux'
      'time-sink'
      'tower'
      'transmission'
      'ttscoff-mmd-quicklook'
      'tuntap'
      'vagrant'
      'vagrant-manager'
      'virtualbox'
      'virtualbox-extension-pack'
      'vlc'
      'waltr'
      'webpquicklook'
      'witch'
      'xee'
      'xquartz'
      'yasu'
      'zotero'
    )

    for index in ${!cask_desired_formulae[*]}; do
      if ! contains_element "${cask_desired_formulae[$index]}" "${__cask_list[@]}"; then
        cask_missing_formulae=("${cask_missing_formulae[@]}" "${cask_desired_formulae[$index]}")
      fi
    done

    if [[ "$cask_missing_formulae" ]]; then
      e_header "Installing missing Homebrew Cask formulae..."

      for item in "${cask_missing_formulae[@]}"; do
        e_header "Installing $item..."
        case "$item" in
          *)
            brew cask install $item
        esac
      done

      [[ $? ]] && e_success "Done"
    fi

    # for appdir in /usr/local/caskroom/*; do
    #   [ -d "$appdir" ] || continue
    #   app="${appdir##*/}"

    #   verlocal="$(find "$appdir"/* -type d -print -quit)"
    #   verlocal="${verlocal##*/}"
    #   verlatest="$(brew cask info "$app" | awk -v app="$app" '$1 == app":"{print $2;}')"

    #   case "$apps" in
    #     *"$sentinel$app$sentinel"*)
    #       if [ "$verbose" -ne 0 ]; then
    #         printf -- ':: found app: %s\n' "$app"
    #         printf -- 'local  version: %s\n' "$verlocal"
    #         printf -- 'latest version: %s\n' "$verlatest"
    #       fi

    #       if [ "$latest" -ne 0 ] && [ "$verlocal" = 'latest' ] || [ "$verlocal" != "$verlatest" ]; then
    #         brew cask install --force "$app" && [ "$verlocal" != "$verlatest" ] && rm -rf "${appdir}/${verlocal}"
    #       fi
    #       ;;
    #     *)
    #       printf -- 'app not found: %s\n' "$app"
    #       ;;
    #   esac
    # done

    # function cask_update() {
    #   rm -rf "$(brew --cache)"
    #   local caskApps=$(ls $BREW_HOME/caskroom) # Lists the casks in the Caskroom

    #   for app in ${caskApps}; do # For every app there, do this
    #     appToCheck=$(brew cask list | grep "${app}") # If the app is not present in `brew cask list`, this variable will be empty

    #     if [[ -z "${appToCheck}" ]]; then # If the variable is empty, then
    #       brew cask install --force "${app}" # Force an install of the app
    #     fi
    #   done
    # }

    # function cask_reinstall() {
    #   rm -rf "$(brew --cache)"

    #   for app in $(brew cask list); do
    #     brew cask install --force "${app}"
    #   done
    # }

    # Remove outdated versions from the Cellar
    brew cleanup
  else
    printf "\n"
    e_error "Error: Homebrew not found."
    printf "Aborting...\n"
    exit
  fi
}
run_brew
