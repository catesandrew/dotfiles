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

    local -a missing_taps
    local -a desired_taps=(
      'caskroom/cask'
      'caskroom/drivers'
      'caskroom/fonts'
      'caskroom/versions'
      'dart-lang/dart'
      'discoteq/discoteq'
      'homebrew/boneyard'
      'homebrew/bundle'
      'homebrew/core'
      'homebrew/services'
      'jbarlow83/ocrmypdf'
      'johanhaleby/kubetail'
      'neovim/neovim'
      'thoughtbot/formulae'
      'universal-ctags/universal-ctags'
      'bramstein/webfonttools'
      'd12frosted/emacs-plus'
    )

    for index in ${!desired_taps[*]}; do
      brew tap "${desired_taps[$index]}" >/dev/null
    done

    for index in ${!desired_taps[*]}; do
      if ! contains_element "${desired_taps[$index]}" "${__brew_taps[@]}"; then
        missing_taps=("${missing_taps[@]}" "${desired_taps[$index]}")
      fi
    done

    # Log unfound taps so user may untap them
    for index in ${!desired_taps[*]}; do
      for (( i=0; i<${#__brew_taps[@]}; i++ )); do
        if [[ ${__brew_taps[i]} == "${desired_taps[$index]}" ]]; then
          __brew_taps=( "${__brew_taps[@]:0:$i}" "${__brew_taps[@]:$((i + 1))}" )
        fi
      done
    done

    if [[ "$__brew_taps" ]]; then
      e_header "Installed but not desired Homebrew taps..."

      for (( i=0; i<${#__brew_taps[@]}; i++ )); do
        e_warning "${__brew_taps[i]} tapped but not desired."
      done
    fi

    if [[ "$missing_taps" ]]; then
      e_header "Tapping missing Homebrew taps..."

      for item in "${missing_taps[@]}"; do
        e_header "Tapping $item..."

        case "$item" in
          *)
            brew tap $item
        esac
      done

      [[ $? ]] && e_success "Done"
    fi

    e_header "Updating any existing Homebrew formulae..."
    # Upgrade any already-installed formulae
    brew upgrade
    [[ $? ]] && e_success "Done"

    e_header "Checking status of desired Homebrew formulae..."
    local __brew_list=($(brew list | sed 's/:.*//'))

    # Use the following to build list ordered by dependencies
    # brew list \
    #   | while read li; do echo -n "$li "; echo $(brew deps "$li"); done \
    #   | awk 'NF == 1 {print $1, $1} NF > 1 {for (i=1;i<=NF;i++) print $1, $i}' \
    #   | tsort \
    #   | tac \
    #   | while read li; do echo "      '$li'"; done

    local -a missing_formulae
    local -a desired_formulae=(
      'libpng'
      'gmp'
      'libffi'
      'libgpg-error'
      'freetype'
      'gettext'
      'pcre'
      'readline'
      'jpeg'
      'libtasn1'
      'libunistring'
      'nettle'
      'p11-kit'
      'libassuan'
      'fontconfig'
      'glib'
      'pixman'
      'gdbm'
      'openssl'
      'sqlite'
      'xz'
      'libtiff'
      'adns'
      'gnutls'
      'libgcrypt'
      'libksba'
      'libusb'
      'npth'
      'pinentry'
      'cairo'
      'pkg-config'
      'python'
      'python3'
      'webp'
      'doxygen'
      'little-cms2'
      'ocaml'
      'lame'
      'x264'
      'xvid'
      'gnupg'
      'libogg'
      'gobject-introspection'
      'graphite2'
      'icu4c'
      'mpfr'
      'docbook'
      'libyaml'
      'autoconf'
      'qt'
      'sip'
      'gd'
      'libtool'
      'openjpeg'
      'clingo'
      'ocamlbuild'
      'leptonica'
      'ffmpeg'
      'gpgme'
      'flac'
      'libid3tag'
      'libvorbis'
      'mad'
      'boost'
      'harfbuzz'
      'jasper'
      'libevent'
      'isl'
      'libmpc'
      'sdl'
      'oniguruma'
      'juju'
      'node'
      'docbook-xsl'
      'gnu-getopt'
      'cabextract'
      'p7zip'
      'unrar'
      'mktorrent'
      'libidn2'
      'berkeley-db'
      'bdw-gc'
      'ucl'
      'libnet'
      'libdnet'
      'ruby'
      'net-snmp'
      'libssh2'
      'coreutils'
      'ruby-build'
      'libquvi'
      'pyqt'
      'py2cairo'
      'pyenv'
      'graphviz'
      'libpq'
      'poppler'
      'ilmbase'
      'aspcud'
      'camlp4'
      'ghostscript'
      'libjpeg'
      'qpdf'
      'tesseract'
      'unpaper'
      'zlib'
      'gmime'
      'talloc'
      'xapian'
      'c-ares'
      'jansson'
      'jemalloc'
      'libev'
      'libtermkey'
      'libuv'
      'libvterm'
      'luajit'
      'msgpack'
      'unibilium'
      'imagemagick'
      'screenresolution'
      'libmp3splt'
      'protobuf'
      'gsasl'
      'cscope'
      'popt'
      'libgsf'
      'librevenge'
      'libsvg'
      'gdk-pixbuf'
      'libcroco'
      'pango'
      'libusb-compat'
      'faac'
      'fdk-aac'
      'libvpx'
      'opus'
      'fribidi'
      'boost-python'
      'kubernetes-cli'
      'mozjpeg'
      'mysql'
      'lua'
      'netpbm'
      'atk'
      'hicolor-icon-theme'
      'pth'
      'go'
      'libidn'
      'unbound'
      'gcc'
      'libexif'
      'libagg'
      'docker'
      'docker-machine'
      'wxmac'
      'zeromq'
      'awscli'
      'jq'
      'juju-wait'
      'pwgen'
      'redis'
      'libsigsegv'
      'charm'
      'mercurial'
      'neon'
      'ghc'
      'ssdeep'
      'dialog'
      'bash'
      'zopfli'
      'yasm'
      'yarn'
      'xmlto'
      'xmlstarlet'
      'x265'
      'wrk'
      'woff2'
      'winetricks'
      'wine'
      'whatmp3'
      'wget'
      'webkit2png'
      'webalizer'
      'wdiff'
      'watchman'
      'watch'
      'w3m'
      'vcprompt'
      'vbindiff'
      'vault'
      'vagrant-completion'
      'utf8proc'
      'urlview'
      'upx'
      'unzip'
      'unrtf'
      'universal-ctags'
      'unison'
      'typescript'
      'txt2tags'
      'two-lame'
      'tvnamer'
      'ttfautohint'
      'tree'
      'trash'
      'transmission'
      'trace2html'
      'tor'
      'tnef'
      'tmuxinator-completion'
      'tmux'
      'tig'
      'tidy-html5'
      'theora'
      'thefuck'
      'the_silver_searcher'
      'the_platinum_searcher'
      'texinfo'
      'texi2html'
      'terminal-notifier'
      'tcptraceroute'
      'tcptrace'
      'tcpreplay'
      'tcpflow'
      'task-spooler'
      'taglib'
      'tag'
      'surfraw'
      'stow'
      'stoken'
      'sslmate'
      'ssh-copy-id'
      'sqlmap'
      'sphinx-doc'
      'spdylay'
      'spark'
      'socat'
      'smartypants'
      'sl'
      'shellcheck'
      'shared-mime-info'
      'sfnt2woff-zopfli'
      'sfnt2woff'
      'serf'
      'selenium-server-standalone'
      'selecta'
      'sdl_image'
      'scons'
      'scheme48'
      'sbcl'
      'sane-backends'
      'rust'
      'rtmpdump'
      'rtf2latex2e'
      'rsync'
      'roswell'
      'rlwrap'
      'rethinkdb'
      'renameutils'
      'rename'
      'recode'
      'reattach-to-user-namespace'
      're2c'
      'rcm'
      'rbenv'
      'ranger'
      'quvi'
      'qscintilla2'
      'pygobject3'
      'pyenv-virtualenv'
      'pv'
      'purescript'
      'pstree'
      'potrace'
      'postgresql'
      'poster'
      'poco'
      'pngquant'
      'pngnq'
      'pngcrush'
      'pngcheck'
      'pmd'
      'plantuml'
      'pinentry-mac'
      'pigz'
      'phantomjs'
      'pgcli'
      'pdfgrep'
      'pdfcrack'
      'passpie'
      'parallel'
      'par'
      'pandoc'
      'packer-completion'
      'packer'
      'otto'
      'orc'
      'optipng'
      'openssh'
      'openh264'
      'openexr'
      'openconnect'
      'open-completion'
      'opam'
      'ocrmypdf'
      'nvm'
      'notmuch'
      'node@6'
      'node@4'
      'nmap'
      'nghttp2'
      'netcat'
      'neovim'
      'neofetch'
      'ncdu'
      'nasm'
      'mycli'
      'mupdf-tools'
      'multitail'
      'multimarkdown'
      'mtr'
      'mr'
      'mosh'
      'mobile-shell'
      'minio-mc'
      'media-info'
      'mdv'
      'md5sha1sum'
      'maven'
      'mas'
      'makedepend'
      'make'
      'mailutils'
      'macvim'
      'mackup'
      'm4'
      'lzo'
      'lzlib'
      'lynx'
      'lxc'
      'logrotate'
      'lmdb'
      'llvm'
      'little-cms'
      'litmus'
      'liquidprompt'
      'lighttpd'
      'libzip'
      'libxml2'
      'libwpd'
      'libwmf'
      'libwebm'
      'libvo-aacenc'
      'libsvg-cairo'
      'libssh'
      'libsodium'
      'librsvg'
      'libnfs'
      'liblqr'
      'libiscsi'
      'libiconv'
      'libicns'
      'libgphoto2'
      'libdvdcss'
      'libcaca'
      'libav'
      'libatomic_ops'
      'libass'
      'lesspipe'
      'less'
      'leiningen'
      'ledger'
      'ldapvi'
      'lcdf-typetools'
      'launchctl-completion'
      'latexml'
      'latex2rtf'
      'lastpass-cli'
      'languagetool'
      'kubetail'
      'kubernetes-helm'
      'kops'
      'known_hosts'
      'knock'
      'jsonpp'
      'jsonlint'
      'jshon'
      'jpegoptim'
      'jpeginfo'
      'jpeg-turbo'
      'jpeg-archive'
      'john'
      'jo'
      'jbig2dec'
      'irssi'
      'intltool'
      'imagemagick@6'
      'id3tool'
      'hydra'
      'hunspell'
      'hub'
      'httrack'
      'httpie'
      'htmlcleaner'
      'html-xml-utils'
      'highlight'
      'hidapi'
      'haskell-stack'
      'hashpump'
      'haproxy'
      'handbrake'
      'hachoir-metadata'
      'gzip'
      'gts'
      'gtk+'
      'gringo'
      'grep'
      'grc'
      'graphicsmagick'
      'gradle'
      'gpg-agent'
      'gpac'
      'godep'
      'gnuplot'
      'gnupg2'
      'gnu-which'
      'gnu-time'
      'gnu-tar'
      'gnu-sed'
      'gnu-indent'
      'global'
      'git-tracker'
      'git-subrepo'
      'git-lfs'
      'git-hooks'
      'git-flow-avh'
      'git-extras'
      'git'
      'gist'
      'gifsicle'
      'giflib'
      'getdns'
      'geoip'
      'gawk'
      'fzf'
      'fswatch'
      'fpp'
      'fortune'
      'foremost'
      'fontforge'
      'flock'
      'findutils'
      'figlet'
      'fftw'
      'fcrackzip'
      'fasd'
      'faad2'
      'expect'
      'expat'
      'exiftool'
      'exiftags'
      'exif'
      'exact-image'
      'enscript'
      'emacs'
      'emacs-plus'
      'elinks'
      'editorconfig'
      'ecj'
      'dwdiff'
      'duti'
      'drip'
      'dos2unix'
      'docutils'
      'docker-swarm'
      'docker-gen'
      'docker-credential-helper'
      'docker-compose'
      'docker-cloud'
      'docker-clean'
      'dnsmasq'
      'dns2tcp'
      'djvulibre'
      'django-completion'
      'dirmngr'
      'direnv'
      'diffutils'
      'diff-so-fancy'
      'diff-pdf'
      'diction'
      'dex2jar'
      'deisctl'
      'dcraw'
      'dbus'
      'dateutils'
      'czmq'
      'curlish'
      'curl'
      'cunit'
      'csv-fix'
      'cowsay'
      'consul-template'
      'consul'
      'conjure-up'
      'colordiff'
      'codequery'
      'cnats'
      'cmake'
      'closure-stylesheets'
      'closure-compiler'
      'cloog'
      'cloc'
      'clisp'
      'clib'
      'clasp'
      'cifer'
      'chromedriver'
      'chrome-cli'
      'chicken'
      'cheat'
      'charm-tools'
      'ccat'
      'carthage'
      'cadaver'
      'cabal-install'
      'bzip2'
      'brew-cask-completion'
      'boot-clj'
      'bison'
      'binwalk'
      'binutils'
      'bind'
      'bibutils'
      'bfg'
      'bcrypt'
      'bazaar'
      'bashish'
      'bash-git-prompt'
      'bash-completion@2'
      'babel'
      'automake'
      'autoconf-archive'
      'aspell'
      'asciinema'
      'asciidoc'
      'archey'
      'apparix'
      'ant'
      'ansiweather'
      'ansible'
      'aircrack-ng'
      'ucspi-tcp'
      'ack'
      'm-cli'
    )

    for index in ${!desired_formulae[*]}; do
      if ! contains_element "${desired_formulae[$index]}" "${__brew_list[@]}"; then
        # Store the name (and options) of every missing formula
        missing_formulae=("${missing_formulae[@]}" "${desired_formulae[$index]}")
      fi
    done

    # Log unfound packages so user may uninstall them
    for index in ${!desired_formulae[*]}; do
      for (( i=0; i<${#__brew_list[@]}; i++ )); do
        if [[ ${__brew_list[i]} == "${desired_formulae[$index]}" ]]; then
          __brew_list=( "${__brew_list[@]:0:$i}" "${__brew_list[@]:$((i + 1))}" )
        fi
      done
    done

    if [[ "$__brew_list" ]]; then
      e_header "Installed but not desired Homebrew formulae..."

      for (( i=0; i<${#__brew_list[@]}; i++ )); do
        e_warning "${__brew_list[i]} installed but not desired."
      done
    fi

    if [[ "$missing_formulae" ]]; then
      e_header "Installing missing Homebrew formulae..."

      for item in "${missing_formulae[@]}"; do
        e_header "Installing $item..."
        case "$item" in
          emacs)
            brew install emacs --HEAD --with-cocoa --with-gnutls --with-imagemagick@6 --with-librsvg --with-modules;
            ;;
          emacs-plus)
            brew install emacs-plus --HEAD --with-24bit-color
            brew linkapps emacs-plus
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
          wdiff)
            brew install wdiff --with-gettext
            ;;
          ffmpeg)
            brew install ffmpeg --with-fdk-aac --with-freetype --with-libass --with-libvpx --with-x265 --with-fdk-aac --with-openh264 --with-openssl --with-rtmpdump --with-theora --with-tools --with-two-lame --with-webp
            ;;
          wget)
            brew install wget --HEAD --with-gpgme --with-pcre --with-iri
            ;;
          macvim)
            brew install macvim --HEAD --with-override-system-vim --with-lua --with-luajit
            ;;
          neovim)
            brew install neovim --HEAD
            ;;
          imagemagick)
            brew install imagemagick --with-webp
            ;;
          *)
            brew install $item
        esac
      done

      # Convert the array of missing formula into a list of space-separate strings
      # local list_formulae=$( printf "%s " "${missing_formulae[@]}" )

      # e_header "Installing missing Homebrew formulae..."
      # brew install $list_formulae

      [[ $? ]] && e_success "Done"
    fi

    __cask_list=($(brew cask list | sed 's/:.*//'))
    local -a cask_missing_formulae
    local -a cask_desired_formulae=(
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

run_mas() {
  if type_exists 'mas'; then
    e_header "Updating apple store apps..."

    local __mas_apps=($(mas list | awk '{print $1}'))
    local __mas_app_names=( $(mas list | awk '{print $2}') )

    local -a mas_missing_apps
    local -a mas_desired_apps=(
      '891059848'
      '511114225'
      '489880259'
      '607529010'
      '435989461'
      '889428659'
      '516801330'
      '524141863'
      '866773894'
      '917665858'
      '457622435'
      '577085396'
      '590518045'
      '634833134'
      '482898991'
      '413965349'
      '615916400'
      '506622388'
      '967805235'
      '525319418'
      '655527594'
      '626221782'
      '920404675'
      '588726889'
      '716854513'
      '671951979'
      '669462988'
      '771501095'
      '966457795'
      '442160773'
      '411643860'
      '970502923'
      '414209656'
      '409203825'
      '607596789'
      '950112142'
      '581789185'
      '609341429'
      '638230527'
      '740472063'
      '557536642'
      '889922906'
      '1161440621'
      '980053405'
      '896450579'
      '931657367'
      '768666595'
      '969418666'
      '439697913'
      '520265986'
      '724472954'
      '555976136'
      '935700987'
      '403504866'
      '905654078'
      '420212497'
      '987247809'
      '979299240'
      '593294811'
      '668208984'
      '1139976917'
      '444990433'
      '445189367'
      '918207447'
      '1156018315'
      '961632517'
      '429449079'
      '924891282'
      '402383384'
      '847496013'
      '522090209'
      '907023335'
      '467939042'
      '451907568'
      '906524969'
      '975937182'
      '948786052'
      '403304796'
      '409183694'
      '568494494'
      '457516296'
      '462227149'
      '407963104'
      '836847379'
      '692815145'
      '1023902784'
      '928871589'
      '536511979'
      '720669838'
      '430798174'
      '936399985'
      '409201541'
      '480623975'
      '1233861775'
      '640841706'
      '459413843'
      '1090940630'
      '642220194'
      '965645209'
      '497799835'
      '824171161'
      '1071676469'
      '671040216'
      '644346785'
      '557090104'
      '476533227'
      '1006087419'
    )

    # Use the following to generally reinstall all apple store applications
    # for i in ${!__mas_app_names[@]}; do
    #   printf "%s\t%s\t%s\n" $i ${__mas_apps[$i]} ${__mas_app_names[$i]}
    #   sudo rm -rf "/Applications/${__mas_app_names[$i]}.app"
    #   mas install ${__mas_apps[$i]};
    # done

    for index in ${!mas_desired_apps[*]}; do
      if ! contains_element "${mas_desired_apps[$index]}" "${__mas_apps[@]}"; then
        mas_missing_apps=("${mas_missing_apps[@]}" "${mas_desired_apps[$index]}")
      fi
    done

    # Log unfound apps so user may uninstall them
    for index in ${!mas_desired_apps[*]}; do
      for (( i=0; i<${#__mas_apps[@]}; i++ )); do
        if [[ ${__mas_apps[i]} == "${mas_desired_apps[$index]}" ]]; then
          __mas_apps=( "${__mas_apps[@]:0:$i}" "${__mas_apps[@]:$((i + 1))}" )
          __mas_app_names=( "${__mas_app_names[@]:0:$i}" "${__mas_app_names[@]:$((i + 1))}" )
        fi
      done
    done

    if [[ "$__mas_apps" ]]; then
      e_header "Installed but not desired apple store apps..."

      for (( i=0; i<${#__mas_apps[@]}; i++ )); do
        e_warning "${__mas_apps[i]} ${__mas_app_names[i]} installed but not desired."
      done
    fi

    if [[ "$mas_missing_apps" ]]; then
      e_header "Installing missing apple store apps..."

      for item in "${mas_missing_apps[@]}"; do
        e_header "Installing $item..."
        case "$item" in
          *)
            mas install "$item"
        esac
      done

      [[ $? ]] && e_success "Done"
    fi
  else
    printf "\n"
    e_error "Error: mas not found."
    printf "Aborting...\n"
    exit
  fi
}
run_brew
run_mas
