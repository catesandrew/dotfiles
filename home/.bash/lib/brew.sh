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

    # If you previously installed OCRmyPDF from this tap please switch to the Homebrew version:
	# brew untap jbarlow83/ocrmypdf
	# brew reinstall ocrmypdf

    local -a missing_taps
    local -a desired_taps=(
      'homebrew/cask-drivers'
      'homebrew/cask-fonts'
      'homebrew/cask-versions'
      'dart-lang/dart'
      'discoteq/discoteq'
      'johanhaleby/kubetail'
      'neovim/neovim'
      'thoughtbot/formulae'
      'universal-ctags/universal-ctags'
      'bramstein/webfonttools'
      'd12frosted/emacs-plus'
      'cloudfoundry/tap'
      'tavianator/tap'
      'zegervdv/zathura'
      'adoptopenjdk/openjdk'
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
      e_header "Installed but not desired Homebrew taps (ignore homebrew/core)..."

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
      'readline'
      'libpng'
      'gdbm'
      'sqlite'
      'xz'
      'jpeg'
      'freetype'
      'gettext'
      'libffi'
      'pcre'
      'python'
      'libogg'
      'libtiff'
      'fontconfig'
      'glib'
      'lzo'
      'pixman'
      'flac'
      'libvorbis'
      'opus'
      'little-cms2'
      'docbook'
      'libunistring'
      'gmp'
      'libevent'
      'cairo'
      'graphite2'
      'icu4c'
      'libsndfile'
      'giflib'
      'openjpeg'
      'webp'
      'docbook-xsl'
      'gnu-getopt'
      'libgpg-error'
      'jasper'
      'libidn2'
      'libtasn1'
      'nettle'
      'p11-kit'
      'unbound'
      'fribidi'
      'harfbuzz'
      'libsamplerate'
      'leptonica'
      'xmlto'
      'libassuan'
      'mpfr'
      'netpbm'
      'nspr'
      'aom'
      'dav1d'
      'frei0r'
      'gnutls'
      'lame'
      'libass'
      'libbluray'
      'libsoxr'
      'libvidstab'
      'libvpx'
      'opencore-amr'
      'rav1e'
      'rtmpdump'
      'rubberband'
      'sdl2'
      'snappy'
      'speex'
      'srt'
      'tesseract'
      'theora'
      'x264'
      'x265'
      'xvid'
      'libde265'
      'shared-mime-info'
      'ilmbase'
      'adns'
      'libgcrypt'
      'libksba'
      'libusb'
      'npth'
      'pinentry'
      'isl'
      'libmpc'
      'autoconf'
      'pkg-config'
      'qt'
      'sip'
      'gd'
      'gts'
      'libtool'
      'pango'
      'krb5'
      'nss'
      'libcbor'
      'ffmpeg'
      'unibilium'
      'ghostscript'
      'libheif'
      'liblqr'
      'libomp'
      'openexr'
      'libyaml'
      'boost'
      'zeromq'
      'gnupg'
      'gcc'
      'hwloc'
      'sdl'
      'gdk-pixbuf'
      'oniguruma'
      'juju'
      'protobuf'
      'ocaml'
      'lua'
      'node'
      'brotli'
      'mktorrent'
      'berkeley-db'
      'ncurses'
      'bdw-gc'
      'jansson'
      'libscrypt'
      'utf8proc'
      'libnet'
      'libdnet'
      'net-snmp'
      'libssh2'
      'lz4'
      'popt'
      'xxhash'
      'zstd'
      'pcre2'
      'coreutils'
      'ruby-build'
      'libquvi'
      'pyqt'
      'gobject-introspection'
      'py3cairo'
      'pyenv'
      'graphviz'
      'libpq'
      'poppler'
      'ldns'
      'libfido2'
      'stoken'
      'jbig2enc'
      'pngquant'
      'pybind11'
      'qpdf'
      'unpaper'
      'c-ares'
      'jemalloc'
      'libev'
      'libtermkey'
      'libuv'
      'libvterm'
      'luajit'
      'msgpack'
      'imagemagick'
      'screenresolution'
      'ninja'
      'gsasl'
      'cscope'
      'ruby'
      'openldap'
      'libgsf'
      'librevenge'
      'libsvg'
      'libid3tag'
      'mad'
      'cunit'
      'libusb-compat'
      'faac'
      'fdk-aac'
      'curl'
      'kubernetes-cli'
      'ipython'
      'pandoc'
      'mozjpeg'
      'libssh'
      'mysql-client'
      'atk'
      'hicolor-icon-theme'
      'go'
      'libcerf'
      'gpgme'
      'bash'
      'libspiro'
      'libuninameslist'
      'libmagic'
      'open-mpi'
      'libexif'
      'libagg'
      'libcroco'
      'librsvg'
      'awscli'
      'jq'
      'juju-wait'
      'pwgen'
      'redis'
      'protobuf-c'
      'libsigsegv'
      'charm'
      'ocamlbuild'
      'neon'
      'ghc'
      'p7zip'
      'ssdeep'
      'json-c'
      'dialog'
      'clingo'
      'source-highlight'
      'zopfli'
      'zlib'
      'ydiff'
      'yasm'
      'yarn'
      'xmlstarlet'
      'xapian'
      'wxmac'
      'wrk'
      'woff2'
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
      'urlview'
      'upx'
      'unzip'
      'unrtf'
      'unrar'
      'universal-ctags'
      'unison'
      'ucl'
      'uchardet'
      'typescript'
      'txt2tags'
      'two-lame'
      'tvnamer'
      'ttfautohint'
      'tree'
      'trash'
      'transmission-cli'
      'tractorgen'
      'trace2html'
      'tor'
      'tnef'
      'tmuxinator-completion'
      'tmux'
      'tig'
      'tidy-html5'
      'thrift'
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
      'talloc'
      'taglib'
      'tag'
      'swig'
      'swaks'
      'surfraw'
      'stow'
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
      'rtf2latex2e'
      'rsync'
      'roswell'
      'rlwrap'
      'ripgrep'
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
      'py2cairo'
      'pv'
      'purescript'
      'pth'
      'pstree'
      'psgrep'
      'potrace'
      'postgresql'
      'poster'
      'poco'
      'pngnq'
      'pngcrush'
      'pngcheck'
      'pmd'
      'plantuml'
      'pinentry-mac'
      'pigz'
      'pgcli'
      'pdfgrep'
      'pdfcrack'
      'passpie'
      'par'
      'packer-completion'
      'packer'
      'orc'
      'optipng'
      'openssh'
      'openh264'
      'openconnect'
      'open-completion'
      'opam'
      'ondir'
      'ocrmypdf'
      'nvm'
      'nmap'
      'nghttp2'
      'netcat'
      'neovim'
      'neofetch'
      'ncdu'
      'nasm'
      'mysql'
      'mycli'
      'mupdf-tools'
      'multitail'
      'multimarkdown'
      'mtr'
      'mr'
      'mpg123'
      'mosh'
      'moreutils'
      'minio-mc'
      'meson'
      'mercurial'
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
      'm-cli'
      'lzlib'
      'lynx'
      'lxc'
      'luarocks'
      'logrotate'
      'lmdb'
      'llvm'
      'little-cms'
      'liquidprompt'
      'lighttpd'
      'libzip'
      'libxml2'
      'libwpd'
      'libwmf'
      'libwebm'
      'libvo-aacenc'
      'libsvg-cairo'
      'libsodium'
      'libnfs'
      'libmp3splt'
      'libjwt'
      'libiscsi'
      'libidn'
      'libiconv'
      'libicns'
      'libgphoto2'
      'libdvdcss'
      'libav'
      'libatomic_ops'
      'lesspipe'
      'less'
      'leiningen'
      'ledger'
      'ldapvi'
      'lcdf-typetools'
      'launchctl-completion'
      'latexml'
      'latex2rtf'
      'latex2html'
      'lastpass-cli'
      'languagetool'
      'kubetail'
      'kops'
      'known_hosts'
      'knock'
      'keychain'
      'jupyterlab'
      'jsonpp'
      'jsonlint'
      'jshon'
      'jpegoptim'
      'jpeginfo'
      'jpeg-turbo'
      'jpeg-archive'
      'john'
      'jo'
      'jenv'
      'jbig2dec'
      'irssi'
      'intltool'
      'id3tool'
      'hydra'
      'hunspell'
      'hub'
      'httrack'
      'httpie'
      'htop'
      'htmlcleaner'
      'html-xml-utils'
      'highlight'
      'hidapi'
      'helm'
      'haskell-stack'
      'hashpump'
      'haproxy'
      'handbrake'
      'gzip'
      'gtk+'
      'grv'
      'grep'
      'grc'
      'graphicsmagick'
      'gradle'
      'gpatch'
      'gpac'
      'godep'
      'go-jira'
      'gnuplot'
      'gnu-which'
      'gnu-time'
      'gnu-tar'
      'gnu-sed'
      'gnu-indent'
      'gmime'
      'git-tracker'
      'git-subrepo'
      'git-lfs'
      'git-hooks'
      'git-flow-avh'
      'git-extras'
      'git'
      'gist'
      'gifsicle'
      'getdns'
      'geoip'
      'gawk'
      'fzf'
      'fswatch'
      'fpp'
      'fortune'
      'foremost'
      'fonttools'
      'fontforge'
      'flock'
      'findutils'
      'file-formula'
      'figlet'
      'fftw'
      'fcrackzip'
      'fastlane'
      'fasd'
      'faad2'
      'expect'
      'expat'
      'exiftool'
      'exiftags'
      'exif'
      'exact-image'
      'enscript'
      'enca'
      'emacs-plus@27'
      'elinks'
      'editorconfig'
      'ed'
      'dwdiff'
      'duti'
      'doxygen'
      'dos2unix'
      'docutils'
      'docker-swarm'
      'docker-machine'
      'docker-gen'
      'docker-credential-helper'
      'docker-compose'
      'docker-cloud'
      'docker-clean'
      'docker'
      'dnsmasq'
      'dns2tcp'
      'djvulibre'
      'django-completion'
      'direnv'
      'diffutils'
      'diff-so-fancy'
      'diction'
      'dex2jar'
      'deisctl'
      'dcraw'
      'dbus'
      'dateutils'
      'dashing'
      'czmq'
      'curlish'
      'csv-fix'
      'cowsay'
      'convox'
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
      'cifer'
      'chrome-cli'
      'chicken'
      'cheat'
      'charm-tools'
      'cf-cli'
      'ccat'
      'carthage'
      'camlp4'
      'cadaver'
      'cabextract'
      'cabal-install'
      'bzip2'
      'brew-cask-completion'
      'boot-clj'
      'boost-python'
      'bison'
      'binwalk'
      'binutils'
      'bind'
      'bibutils'
      'bfs'
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
      'aspcud'
      'asciinema'
      'asciidoc'
      'archey'
      'apparix'
      'apktool'
      'ant'
      'ansiweather'
      'ansible'
      'aircrack-ng'
      'ack'
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
          hunspell)
            # download dictionaries from http://wordlist.aspell.net/dicts/, insall in ~/Library/Spelling/
            brew install hunspell
            ;;
          emacs)
            # --with-gnutls has issues with gh-api
            brew install emacs --HEAD --with-cocoa --with-imagemagick@6 --with-librsvg --with-modules;
            ;;
          emacs-plus@27)
            # emacs-plus issues with daemon mode, better color emoji support
            brew install emacs-plus@27 --HEAD --with-jansson --with-spacemacs-icon
            ;;
          wget)
            brew install wget --HEAD
            ;;
          macvim)
            # sudo xcode-select -s /Applications/Xcode.app/Contents/Developer
            brew install macvim --HEAD
            ;;
          neovim)
            brew install neovim --HEAD
            ;;
          universal-ctags)
            # Given the lack of activity on the official Exuberant Ctags
            # source, it has been forked  and renamed to Universal Ctags
            # and can be found at universal-ctags/ctags.
            brew install universal-ctags/universal-ctags/universal-ctags --HEAD --with-jansson --with-libyaml
            ;;
          meson)
            # A bug in the latest stable version was fixed but has not been released yet.
            brew install meson --HEAD
            ;;
          zathura)
            brew install zathura
            # Follow the instructions to link the plugins into place:
            mkdir -p $(brew --prefix zathura)/lib/zathura
            ;;
          zathura-pdf-poppler)
            brew install zathura-pdf-poppler
            # Follow the instructions to link the plugins into place:
            ln -s $(brew --prefix zathura-pdf-poppler)/lib/pdf.dylib $(brew --prefix zathura)/lib/zathura/pdf.so
            ;;
          rbenv)
            brew install rbenv ruby-build
            # rbenv install 2.6.5
            # rbenv global 2.6.5
            # gem install -n /usr/local/bin fastlane -NV
            ;;
          moreutils)
            brew unlink parallel task-spooler && brew install moreutils &&  brew link --overwrite task-spooler parallel
            ;;
          node)
            brew insall node
            # load npm completion from nvm
            rm "${BREW_HOME}/etc/bash_completion.d/npm"
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
      '1password'
      '1password-cli'
      'adoptopenjdk14'
      'adoptopenjdk8'
      'aerial'
      'altair-graphql-client'
      'android-ndk'
      'android-sdk'
      'app-tamer'
      'audio-hijack'
      'bartender'
      'beamer'
      'bettertouchtool'
      'betterzip'
      'charles'
      'chromedriver'
      'cloudapp'
      'coderunner'
      'curio'
      'dash'
      'eaglefiler'
      'electron'
      'epubmdimporter'
      'epubquicklook'
      'facebook-ios-sdk'
      'fastlane'
      'fliqlo'
      'font-open-iconic'
      'font-open-sans'
      'font-raleway'
      'font-xits'
      'forklift'
      'genymotion'
      'gitup'
      'google-chrome'
      'google-chrome-canary'
      'graphiql'
      'graphql-ide'
      'graphql-playground'
      'hammerspoon'
      'handbrake'
      'hazel'
      'houdahspot'
      'iconjar'
      'imagealpha'
      'imagemin'
      'imageoptim'
      'intel-haxm'
      'iterm2'
      'karabiner-elements'
      'keepassx'
      'keyboard-maestro'
      'launchbar'
      'launchcontrol'
      'leech'
      'licecap'
      'lightpaper'
      'macpass'
      'mactex'
      'mailmate'
      'markdownmdimporter'
      'mattermost'
      'mediainfo'
      'moom'
      'name-mangler'
      'netspot'
      'pacifist'
      'paw'
      'pdf-expert'
      'pdfkey-pro'
      'pdfpenpro'
      'phantomjs'
      'platypus'
      'plistedit-pro'
      'postman'
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
      'rcdefaultapp'
      'react-native-debugger'
      'reactotron'
      'resolutionator'
      'scapple'
      'screenflow'
      'scrivener'
      'selfcontrol'
      'sigil'
      'skim'
      # 'slack'
      # 'sonos'
      'spamsieve'
      'suspicious-package'
      'textexpander'
      'touch-bar-simulator'
      'tower2'
      'transmission'
      'ttscoff-mmd-quicklook'
      'tuntap'
      'vagrant'
      'vagrant-manager'
      'virtualbox'
      'virtualbox-extension-pack'
      'webpquicklook'
      'witch'
      'xee'
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
          android-sdk)
            # https://stackoverflow.com/questions/46402772/failed-to-install-android-sdk-java-lang-noclassdeffounderror-javax-xml-bind-a
            brew cask intsall android-sdk
            # Update 2019-10: Google is working on a new Android SDK
            # Command-line Tools release that runs on current JVMs (9, 10, 11+)
            # and does not depend on deprecated JAXB EE modules!
            #
            # https://dl.google.com/android/repository/commandlinetools-mac-5842447_latest.zip
            # You can already download and use the new Android SDK Command-line
            # Tools on the canary channel inside Android Studio or by manually
            # downloading them from the Google servers:
            #
            # For the latest versions check the URLs inside the
            # https://dl.google.com/android/repository/repository2-1.xml.
            ;;
          android-ndk)
            brew cask intsall android-ndk
            yes | sdkmanager --licenses
            # Install all of the Android SDK components (you will be prompted to
            # agree to license info and then this will take a while to run):
            touch "${HOME}/.android/repositories.cfg"
            # accept the licenses
            yes | sdkmanager --licenses
            sdkmanager --update
            sdkmanager --no_https --install emulator
            sdkmanager --no_https --install platform-tools
            sdkmanager --no_https --install 'system-images;android-29;google_apis_playstore;x86_64'
            sdkmanager --no_https --install 'extras;intel;Hardware_Accelerated_Execution_Manager'
            sdkmanager --no_https --install 'build-tools;29.0.2'
            sdkmanager --no_https --install 'platforms;android-29'
            sdkmanager --list
            ;;
          adoptopenjdk8)
            # android-sdk requires Java 8. You can install it with:
            brew cask install homebrew/cask-versions/adoptopenjdk8
            jenv add "$(/usr/libexec/java_home -v1.8)"
            # jenv global 1.8 # To unset the version `jenv global system`
            ;;
          adoptopenjdk14)
            brew cask install adoptopenjdk14
            mkdir -p "${HOME}/.jenv/versions"
            jenv add "$(/usr/libexec/java_home -v14)"
            jenv global 14 # To unset the version `jenv global system`

            # remove openjdk if installed `brew uninstall --ignore-dependencies openjdk`
            # but it is required by ant, bfg, boot-clj, closure-compiler,
            # closure-stylesheets, gradle, languagetool, maven, plantuml, pmd
            # and selenium-server-standalone
            ;;
          java6)
            brew cask install java6
            jenv add "$(/usr/libexec/java_home -v1.6)"
            ;;
          java7)
            brew cask install java7
            jenv add "$(/usr/libexec/java_home -v1.7)"
            ;;
          java8)
            brew cask install java8
            jenv add "$(/usr/libexec/java_home -v1.8)"
            ;;
          java9)
            brew cask install java9
            jenv add "$(/usr/libexec/java_home -v9)"
            ;;
          *)
            brew cask install "${item}"
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
        mas info "$item"
        case "$item" in
          889428659)
            # xScope
            mas install 889428659
            # Hiding the xScope Dock icon
            # https://support.iconfactory.com/kb/xscope/hiding-the-xscope-dock-icon
            defaults write com.iconfactory.mac.xScope generalShowDockIcon 0
            defaults write com.iconfactory.xScope generalShowDockIcon 0
            ;;
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
