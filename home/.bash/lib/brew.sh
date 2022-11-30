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

e_install() {
  printf "\ninstalling $(tput setaf 4)%s$(tput sgr0)\n\n" "$@"
}

command_exists () {
  type "$1" &> /dev/null ;
}

installed () {
  echo -e " ✓ $1 already installed."
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
      'borkdude/brew'
      'bramstein/webfonttools'
      'catesandrew/tap'
      'clojure/tools'
      'cloudfoundry/tap'
      'd12frosted/emacs-plus'
      'dart-lang/dart'
      'discoteq/discoteq'
      'facebook/fb'
      'heroku/brew'
      'homebrew/cask'
      'homebrew/cask-drivers'
      'homebrew/cask-fonts'
      'homebrew/cask-versions'
      'homebrew/core'
      'homebrew/services'
      'johanhaleby/kubetail'
      'launchdarkly/tap'
      'lindell/multi-gitter'
      'neovim/neovim'
      'saucelabs/saucectl'
      'tavianator/tap'
      'thoughtbot/formulae'
      'universal-ctags/universal-ctags'
      'zegervdv/zathura'
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
    # brew list --formula \
    #   | while read li; do echo -n "$li "; echo $(brew deps --formula --installed "$li"); done \
    #   | awk 'NF == 1 {print $1, $1} NF > 1 {for (i=1;i<=NF;i++) print $1, $i}' \
    #   | tsort \
    #   | tac \
    #   | while read li; do echo "      '$li'"; done

    local -a missing_formulae
    local -a desired_formulae=(
      'pkg-config'
      'xorgproto'
      'libpthread-stubs'
      'libxau'
      'libxdmcp'
      'lz4'
      'xz'
      'ca-certificates'
      'jpeg-turbo'
      'libpng'
      'libxcb'
      'icu4c'
      'zstd'
      'gflags'
      'openssl@1.1'
      'imath'
      'giflib'
      'libtiff'
      'freetype'
      'gettext'
      'pcre2'
      'libx11'
      'libogg'
      'm4'
      'boost'
      'double-conversion'
      'fmt'
      'glog'
      'libevent'
      'snappy'
      'brotli'
      'openexr'
      'webp'
      'fontconfig'
      'glib'
      'libxext'
      'libxrender'
      'lzo'
      'pixman'
      'flac'
      'libvorbis'
      'opus'
      'little-cms2'
      'bdw-gc'
      'gmp'
      'libtool'
      'libunistring'
      'readline'
      'libtasn1'
      'libnghttp2'
      'libgpg-error'
      'docbook'
      'folly'
      'libsodium'
      'jpeg-xl'
      'libvmaf'
      'cairo'
      'graphite2'
      'libsndfile'
      'openjpeg'
      'libb2'
      'guile'
      'libidn2'
      'nettle'
      'p11-kit'
      'unbound'
      'libassuan'
      'docbook-xsl'
      'gnu-getopt'
      'mpfr'
      'fizz'
      'aom'
      'jasper'
      'fribidi'
      'harfbuzz'
      'cjson'
      'cmocka'
      'mbedtls'
      'libsamplerate'
      'leptonica'
      'libarchive'
      'gnutls'
      'libgcrypt'
      'libksba'
      'libusb'
      'npth'
      'pinentry'
      'xmlto'
      'isl'
      'libmpc'
      'wangle'
      'libavif'
      'netpbm'
      'gdk-pixbuf'
      'pango'
      'gdbm'
      'mpdecimal'
      'sqlite'
      'nspr'
      'dav1d'
      'frei0r'
      'lame'
      'libass'
      'libbluray'
      'librist'
      'libsoxr'
      'libvidstab'
      'libvpx'
      'opencore-amr'
      'rav1e'
      'rubberband'
      'sdl2'
      'speex'
      'srt'
      'tesseract'
      'theora'
      'x264'
      'x265'
      'xvid'
      'zeromq'
      'zimg'
      'gnupg'
      'libcbor'
      'gsettings-desktop-schemas'
      'jbig2dec'
      'libidn'
      'libde265'
      'shared-mime-info'
      'libomp'
      'gcc'
      'fbthrift'
      'gd'
      'gts'
      'librsvg'
      'autoconf'
      'assimp'
      'dbus'
      'hunspell'
      'libmng'
      'md4c'
      'krb5'
      'nss'
      'libimagequant'
      'libraqm'
      'tcl-tk'
      'ffmpeg'
      'gpgme'
      'berkeley-db'
      'libssh2'
      'openldap'
      'rtmpdump'
      'libfido2'
      'zlib'
      'atk'
      'hicolor-icon-theme'
      'libepoxy'
      'graphene'
      'gstreamer'
      'orc'
      'glib-networking'
      'libpsl'
      'libcuefile'
      'libreplaygain'
      'hwloc'
      'sdl12-compat'
      'ghostscript'
      'libheif'
      'liblqr'
      'libraw'
      'gsasl'
      'libyaml'
      'openblas'
      'lua'
      'oniguruma'
      'mktorrent'
      'edencommon'
      'fb303'
      'ncurses'
      'graphviz'
      'jansson'
      'libscrypt'
      'utf8proc'
      'pcre'
      'libnet'
      'libdnet'
      'docutils'
      'net-snmp'
      'openssl@3'
      'popt'
      'xxhash'
      'protobuf'
      'coreutils'
      'sip'
      'qt'
      'gobject-introspection'
      'pyenv'
      'python@3.10'
      'py3cairo'
      'libpython-tabulate'
      'zopfli'
      'libpq'
      'poppler'
      'qrencode'
      'tree'
      'pandoc'
      'ldns'
      'stoken'
      'gpatch'
      'jbig2enc'
      'pillow'
      'pngquant'
      'pybind11'
      'qpdf'
      'unpaper'
      'ocaml'
      'gmime'
      'talloc'
      'xapian'
      'c-ares'
      'jemalloc'
      'libev'
      'screenresolution'
      'tokyo-cabinet'
      'ninja'
      'libuv'
      'libgsf'
      'librevenge'
      'unibilium'
      'libsvg'
      'libid3tag'
      'mad'
      'cunit'
      'libexif'
      'libusb-compat'
      'perl'
      'curl'
      'kubernetes-cli'
      'juju'
      'mozjpeg'
      'libssh'
      'mysql-client'
      'heroku/brew/heroku-node'
      'gtk+'
      'gtk+3'
      'gst-plugins-base'
      'libshout'
      'libsoup'
      'taglib'
      'faac'
      'faad2'
      'fdk-aac'
      'libnice'
      'libusrsctp'
      'musepack'
      'srtp'
      'abseil'
      're2'
      'libcerf'
      'bash'
      'libspiro'
      'libuninameslist'
      'libmagic'
      'open-mpi'
      'libagg'
      'imagemagick'
      'libgccjit'
      'mailutils'
      'rlwrap'
      'libsigsegv'
      'charm'
      'hiredis'
      'neon'
      'ghc'
      'numpy'
      'p7zip'
      'ssdeep'
      'json-c'
      'dialog'
      'clingo'
      'source-highlight'
      'jq'
      'ydiff'
      'yasm'
      'xmlstarlet'
      'xml2'
      'wxwidgets'
      'wrk'
      'woff2'
      'wmctrl'
      'whatmp3'
      'wget'
      'webkit2png'
      'wdiff'
      'watchman'
      'watch'
      'w3m'
      'vpn-slice'
      'vcprompt'
      'vbindiff'
      'vault'
      'vala'
      'vagrant-completion'
      'v8'
      'urlview'
      'unzip'
      'unrtf'
      'universal-ctags'
      'unison'
      'ucl'
      'uchardet'
      'txt2tags'
      'two-lame'
      'tvnamer'
      'ttfautohint'
      'tree-sitter'
      'trash'
      'transmission-cli'
      'tractorgen'
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
      'terraform'
      'terminal-notifier'
      'tcptraceroute'
      'tcptrace'
      'tcpreplay'
      'tcpflow'
      'tag'
      'synctex'
      'switchaudio-osx'
      'swig'
      'swaks'
      'surfraw'
      'stow'
      'sslmate'
      'ssh-copy-id'
      'sqlmap'
      'sphinx-doc'
      'spark'
      'socat'
      'smartypants'
      'slackcat'
      'sl'
      'shellcheck'
      'sfnt2woff-zopfli'
      'sfnt2woff'
      'serf'
      'selenium-server'
      'selecta'
      'sdl_image'
      'scrcpy'
      'scons'
      'scmpuff'
      'scheme48'
      'sbcl'
      'saucectl'
      'sane-backends'
      'rust'
      'rtf2latex2e'
      'rsync'
      'roswell'
      'ripgrep'
      'rethinkdb'
      'renameutils'
      'rename'
      'redis'
      'recode'
      'reattach-to-user-namespace'
      're2c'
      'rcm'
      'rbenv'
      'ruby-build'
      'ruby'
      'ranger'
      'qscintilla2'
      'python-tabulate'
      'pyqt-builder'
      'pyqt'
      'pygobject3'
      'pyenv-virtualenv'
      'pwgen'
      'pv'
      'pth'
      'pstree'
      'psgrep'
      'protobuf-c'
      'potrace'
      'postgresql@14'
      'poster'
      'poco'
      'pngnq'
      'pngcrush'
      'pngcheck'
      'pmd'
      'plotutils'
      'plantuml'
      'pkcs11-helper'
      'pinentry-mac'
      'pigz'
      'pgcli'
      'pdfgrep'
      'pdfcrack'
      'pdf2svg'
      'passpie'
      'pass'
      'par'
      'pandoc-crossref'
      'packer-completion'
      'packer'
      'osx-cpu-temp'
      'optipng'
      'openssh'
      'openh264'
      'openconnect'
      'open-completion'
      'opam'
      'ondir'
      'ocrmypdf'
      'ocamlbuild'
      'nvm'
      'notmuch'
      'nmap'
      'nghttp2'
      'netcat'
      'neofetch'
      'ncdu'
      'nasm'
      'mysql'
      'mycli'
      'mutt'
      'mupdf-tools'
      'multitail'
      'multimarkdown'
      'multi-gitter'
      'mu'
      'mtr'
      'msmtp'
      'msgpack'
      'mr'
      'mpg123'
      'mosh'
      'moreutils'
      'minio-mc'
      'meson'
      'media-info'
      'mdv'
      'maven'
      'mas'
      'makedepend'
      'make'
      'mackup'
      'm-cli'
      'lzlib'
      'lynx'
      'lxc'
      'luv'
      'luarocks'
      'luajit-openresty'
      'luajit'
      'lolcat'
      'logrotate'
      'lmdb'
      'llvm'
      'liquidprompt'
      'lighttpd'
      'libzip'
      'libxml2'
      'libwpd'
      'libwmf'
      'libwebm'
      'libvterm'
      'libvo-aacenc'
      'libtermkey'
      'libsvg-cairo'
      'libproxy'
      'libplist'
      'libnotify'
      'libnfs'
      'libmp3splt'
      'libmms'
      'libmetalink'
      'libjwt'
      'libiscsi'
      'libiconv'
      'libicns'
      'libgphoto2'
      'libffi'
      'libdvdcss'
      'libcroco'
      'libatomic_ops'
      'lesspipe'
      'less'
      'leiningen'
      'ledger'
      'ldapvi'
      'ld-find-code-refs'
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
      'juju-wait'
      'jsonpp'
      'jshon'
      'jpegoptim'
      'jpeginfo'
      'jpeg-archive'
      'jpeg'
      'john'
      'jo'
      'jenv'
      'isync'
      'irssi'
      'ipython'
      'intltool'
      'inframap'
      'ilmbase'
      'idb-companion'
      'id3tool'
      'ical-buddy'
      'hydra'
      'httrack'
      'httpie'
      'htop'
      'htmlcleaner'
      'html-xml-utils'
      'highlight'
      'hidapi'
      'heroku-node'
      'heroku'
      'helm'
      'haskell-stack'
      'hashpump'
      'haproxy'
      'handbrake'
      'gzip'
      'gtk-mac-integration'
      'gst-plugins-ugly'
      'gst-plugins-good'
      'gst-plugins-bad'
      'gst-libav'
      'grpc'
      'groovy'
      'grep'
      'grc'
      'graphicsmagick'
      'gradle'
      'gpac'
      'go-jira'
      'go'
      'gnuplot'
      'gnu-which'
      'gnu-time'
      'gnu-tar'
      'gnu-sed'
      'gnu-indent'
      'git-when-merged'
      'git-tracker'
      'git-subrepo'
      'git-quick-stats'
      'git-lfs'
      'git-flow-avh'
      'git-extras'
      'git-delta'
      'git-crypt'
      'git'
      'gist'
      'gifsicle'
      'gh'
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
      'fd'
      'fcrackzip'
      'zoxide'
      'expect'
      'expat'
      'exiftool'
      'exiftags'
      'exif'
      'exact-image'
      'enscript'
      'enca'
      'emacs-plus@29'
      'tre'
      'felinks'
      'editorconfig'
      'ed'
      'dwdiff'
      'duti'
      'doxygen'
      'dotenv-linter'
      'dos2unix'
      'docker-machine'
      'docker-gen'
      'docker-credential-helper'
      'docker-compose'
      'docker-clean'
      'dnsmasq'
      'dns2tcp'
      'djvulibre'
      'direnv'
      'diffutils'
      'diff-so-fancy'
      'diction'
      'dex2jar'
      'desktop-file-utils'
      'dcraw'
      'dateutils'
      'dashing'
      'dart'
      'czmq'
      'cscope'
      'cowsay'
      'convox'
      'consul-template'
      'consul'
      'colordiff'
      'codequery'
      'cocoapods'
      'cmake'
      'closure-compiler'
      'cloog'
      'clojure'
      'cloc'
      'clisp'
      'cifer'
      'chrome-cli'
      'chicken'
      'charm-tools'
      'cf-cli'
      'ccat'
      'ccache'
      'carthage'
      'cadaver'
      'cabextract'
      'cabal-install'
      'bzip2'
      'brew-cask-completion'
      'boot-clj'
      'bison'
      'binwalk'
      'binutils'
      'bind'
      'bibutils'
      'bfs'
      'bfg'
      'bcrypt'
      'bat'
      'bashish'
      'bash-git-prompt'
      'bash-completion@2'
      'babashka'
      'awscli'
      'automake'
      'aspell'
      'aspcud'
      'asciinema'
      'asciidoc'
      'apparix'
      'apktool'
      'ant'
      'ansiweather'
      'aircrack-ng'
      'adwaita-icon-theme'
      'adns'
      'act'
      'ack'
      'lsd'
      'difftastic'
      'libimobiledevice-glue'
      'libusbmuxd'
      'libimobiledevice'
      'ideviceinstaller'
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

    # for index in ${!desired_formulae[*]}; do
    #   e_header "Installing ${desired_formulae[$index]}..."
    #   brew reinstall "${desired_formulae[$index]}"
    # done

    if [[ "$missing_formulae" ]]; then
      e_header "Installing missing Homebrew formulae..."

      for item in "${missing_formulae[@]}"; do
        e_install "$item"

        case "$item" in
          perl)
            HOMEBREW_NO_INSTALL_CLEANUP=1 brew install cpan --ignore-dependencies
            PERL_MM_OPT="INSTALL_BASE=$HOME/perl5" cpan install local::lib
            ;;
          libimobiledevice-glue)
            HOMEBREW_NO_INSTALL_CLEANUP=1 \
              brew install \
              --ignore-dependencies \
              --HEAD \
              catesandrew/tap/libimobiledevice-glue
            ;;
          libusbmuxd)
            HOMEBREW_NO_INSTALL_CLEANUP=1 \
              brew install \
              --HEAD \
              catesandrew/tap/libusbmuxd
            ;;
          libimobiledevice)
            HOMEBREW_NO_INSTALL_CLEANUP=1 \
              brew install \
              --HEAD \
              catesandrew/tap/libimobiledevice
            ;;
          ideviceinstaller)
            HOMEBREW_NO_INSTALL_CLEANUP=1 \
              brew install \
              --HEAD \
              catesandrew/tap/ideviceinstaller
            ;;
          ruby@3)
            rbenv uninstall 3.1
            rbenv rehash
            brew install --ignore-dependencies ruby@3
            rbenv global 3.1 system

            ln -sfv "$(realpath $(brew --prefix ruby@3))" $RBENV_ROOT/versions/3.1
            pyenv rehash
            ls -al $RBENV_ROOT/versions

            ;;
          python3)
            # https://thecesrom.dev/2021/06/28/how-to-add-python-installed-via-homebrew-to-pyenv-versions/
            pyenv uninstall 3.9.X
            pyenv rehash
            brew install --ignore-dependencies python@3
            pyenv global 3.10 system

            ln -sfv "$(realpath $(brew --prefix python@3.10))" $PYENV_ROOT/versions/3.10
            ln -sfv $(realpath $(brew --prefix python@3.10))/Frameworks/Python.framework/Versions/3.10/include/python3.10 $(realpath $(brew --prefix python@3.10))/include

            ln -sfv $(realpath $(brew --prefix python@3.10))/bin/idle3 $(realpath $(brew --prefix python@3.10))/bin/idle
            ln -sfv $(realpath $(brew --prefix python@3.10))/bin/pip3 $(realpath $(brew --prefix python@3.10))/bin/pip
            ln -sfv $(realpath $(brew --prefix python@3.10))/bin/python3 $(realpath $(brew --prefix python@3.10))/bin/python
            ln -sfv $(realpath $(brew --prefix python@3.10))/bin/wheel3 $(realpath $(brew --prefix python@3.10))/bin/wheel
            pyenv rehash

            ls -al $PYENV_ROOT/versions
            ;;
          awscli)
            HOMEBREW_NO_INSTALL_CLEANUP=1 brew install --ignore-dependencies awscli
            pip3 install six
            pip3 install aws-sso-cred-restore
            ;;
          hunspell)
            # download dictionaries from http://wordlist.aspell.net/dicts/, insall in ~/Library/Spelling/
            HOMEBREW_NO_INSTALL_CLEANUP=1 brew install hunspell --ignore-dependencies
            ;;
          emacs)
            # brew install emacs --HEAD --with-cocoa --with-imagemagick@6 --with-librsvg --with-modules;
            ;;
          gcc|libgccjit)
            # for emacs-plus native-comp support
            HOMEBREW_NO_INSTALL_CLEANUP=1 \
              HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK=1 \
              brew install \
              --ignore-dependencies \
              --build-from-source \
              "${item}"
            ;;
          emacs-plus@29)
            # emacs-plus issues with daemon mode, better color emoji support
            HOMEBREW_NO_INSTALL_CLEANUP=1 \
              LIBRARY_PATH="$(brew --prefix)/lib" \
              brew install \
              --ignore-dependencies \
              emacs-plus@29 \
              --with-native-comp \
              --with-xwidgets \
              --with-imagemagick \
              --with-mailutils \
              --with-poll \
              --with-no-frame-refocus \
              --with-spacemacs-icon
            ;;
          wget)
            HOMEBREW_NO_INSTALL_CLEANUP=1 brew install wget --HEAD
            ;;
          macvim)
            xcode-select --install # Install Command Line Tools if you haven't already.
            # sudo xcode-select --reset
            # sudo xcode-select -s /Applications/Xcode.app/Contents/Developer
            # sudo xcodebuild -license accept
            # HOMEBREW_NO_INSTALL_CLEANUP=1 brew install --ignore-dependencies macvim
            e_warning "Install macvim through cask as to not have to install python dependency"
            ;;
          neovim)
            HOMEBREW_NO_INSTALL_CLEANUP=1 brew install --ignore-dependencies neovim
            ;;
          universal-ctags)
            # Given the lack of activity on the official Exuberant Ctags
            # source, it has been forked and renamed to Universal Ctags
            # and can be found at universal-ctags/ctags.
            HOMEBREW_NO_INSTALL_CLEANUP=1 brew install --HEAD --ignore-dependencies universal-ctags/universal-ctags/universal-ctags
            ;;
          meson)
            # A bug in the latest stable version was fixed but has not been released yet.
            HOMEBREW_NO_INSTALL_CLEANUP=1 brew install --ignore-dependencies meson
            ;;
          zathura)
            # these formula do not install on m1
            # 'girara'
            # 'zathura'
            # 'zathura-pdf-poppler'
            # 'zegervdv/zathura/girara'
            # 'zegervdv/zathura/zathura'
            # 'zegervdv/zathura/synctex'

            HOMEBREW_NO_INSTALL_CLEANUP=1 brew install --ignore-dependencies zathura
            # Follow the instructions to link the plugins into place:
            mkdir -p $(brew --prefix zathura)/lib/zathura
            ;;
          zathura-pdf-poppler)
            HOMEBREW_NO_INSTALL_CLEANUP=1 brew install --ignore-dependencies zathura-pdf-poppler
            # Follow the instructions to link the plugins into place:
            ln -s $(brew --prefix zathura-pdf-poppler)/lib/pdf.dylib $(brew --prefix zathura)/lib/zathura/pdf.so
            ;;
          rbenv)
            HOMEBREW_NO_INSTALL_CLEANUP=1 brew install --ignore-dependencies rbenv
            rbenv install 2.7.4
            rbenv global 2.7.4
            gem install bundler
            ;;
          jenv)
            HOMEBREW_NO_INSTALL_CLEANUP=1 brew install --ignore-dependencies jenv
            mkdir -p "${HOME}/.jenv/versions"
            mkdir -p "${HOME}/.jenv/plugins"
            # jenv enable-plugin lein
            jenv enable-plugin export
            # it is required by ant, bfg, boot-clj, closure-compiler,
            # gradle, languagetool, maven, plantuml, pmd and selenium-server-standalone.
            # jenv add "${BREW_HOME}/opt/openjdk/libexec/openjdk.jdk/Contents/Home"
            # jenv global 11 # To unset the version `jenv global system`
            ;;
          nvm)
            # uninstall node and npm because we are going to install through nvm.
            brew uninstall --ignore-dependencies node npm
            brew uninstall --force node
            # load npm completion from nvm
            # rm "${BREW_HOME}/etc/bash_completion.d/npm"

            # install nvm with latest version of node
            sudo mkdir -p "${BREW_HOME}/nvm"
            sudo chmod -R go+w "${BREW_HOME}/nvm"
            export NVM_DIR="$(realpath "${BREW_HOME}/../nvm")"

            HOMEBREW_NO_INSTALL_CLEANUP=1 brew install --ignore-dependencies nvm
            nvm install v18
            nvm use node
            node_version=$(node --version | sed 's/[^0-9.]*//g')
            nvm alias default $node_version
            ;;
          *)
            HOMEBREW_NO_INSTALL_CLEANUP=1 brew install --ignore-dependencies "${item}"
        esac
      done

      # Convert the array of missing formula into a list of space-separate strings
      # local list_formulae=$( printf "%s " "${missing_formulae[@]}" )

      # e_header "Installing missing Homebrew formulae..."
      # brew install $list_formulae

      [[ $? ]] && e_success "Done"
    fi

    # brew list --cask \
    #  | while read li; do echo -n "$li "; done \
    #  | awk 'NF == 1 {print $1, $1} NF > 1 {for (i=1;i<=NF;i++) print $1, $i}' \
    #  | tsort | tac | while read li; do echo "      '$li'"; done

    __cask_list=($(brew list --cask | sed 's/:.*//'))
    local -a cask_missing_formulae
    local -a cask_desired_formulae=(
      '1password-cli'
      # 'adoptopenjdk11'
      # 'adoptopenjdk8'
      'aerial'
      'altair-graphql-client'
      'android-commandlinetools'
      'android-ndk'
      'android-platform-tools'
      'appium'
      'appium-inspector'
      'araxis-merge'
      'audio-hijack'
      'aws-vault'
      'bartender'
      'bettertouchtool'
      'betterzip'
      'bibdesk'
      'blackhole-2ch'
      'charles'
      'cloudapp'
      'coderunner'
      'dash'
      'deckset'
      'docker'
      'eaglefiler'
      'electron'
      'figma'
      'flipper'
      'fliqlo'
      'font-fontawesome'
      'font-lato'
      'font-open-iconic'
      'font-xits'
      'gimp'
      'gitup'
      'google-chrome'
      'google-chrome-canary'
      'gpg-suite-no-mail'
      'graphiql'
      'graphql-ide'
      'graphql-playground'
      'hammerspoon'
      'hazel'
      'iconjar'
      'imagealpha'
      'imagemin'
      'imageoptim'
      'inkscape'
      'ipe'
      'iterm2'
      'kaleidoscope'
      'karabiner-elements'
      'keyboard-maestro'
      'kitty'
      'launchbar'
      'launchcontrol'
      'mactex'
      'mailmate'
      'marked'
      'mathpix-snipping-tool'
      'mediainfo'
      'meld'
      'moom'
      'name-mangler'
      'ngrok'
      'nosqlbooster-for-mongodb'
      'numi'
      'obs'
      'obsidian'
      'openvpn-connect'
      'pacifist'
      'paw'
      'pdfpenpro'
      'phantomjs'
      'platypus'
      'plistedit-pro'
      'postman'
      'prefs-editor'
      'prince'
      'provisionql'
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
      'rar'
      'react-native-debugger'
      'reactotron'
      'resolutionator'
      'sauce-connect'
      'scapple'
      'screenflow'
      'skim'
      'sonos'
      'spamsieve'
      'suspicious-package'
      'tex-live-utility'
      'textexpander'
      'touch-bar-simulator'
      'tower'
      'transmission'
      'ttscoff-mmd-quicklook'
      'ubersicht'
      # 'vagrant'
      # 'vagrant-manager'
      # 'virtualbox'
      # 'virtualbox-extension-pack'
      'visual-studio-code'
      'vlc'
      'webpquicklook'
      'witch'
      'wkhtmltopdf'
      'xee'
      'zeplin'
      'zotero'
      '1password'
      'macvim'
      'alacritty'
      'temurin8'
      'temurin11'
      'temurin'
      'kdiff3'
    )

    # for index in ${!cask_desired_formulae[*]}; do
    #   e_header "Installing ${cask_desired_formulae[$index]}..."
    #   brew reinstall --cask "${cask_desired_formulae[$index]}"
    # done

    for index in ${!cask_desired_formulae[*]}; do
      if ! contains_element "${cask_desired_formulae[$index]}" "${__cask_list[@]}"; then
        cask_missing_formulae=("${cask_missing_formulae[@]}" "${cask_desired_formulae[$index]}")
      fi
    done

    if [[ "$cask_missing_formulae" ]]; then
      e_header "Installing missing Homebrew Cask formulae..."

      for item in "${cask_missing_formulae[@]}"; do
        e_install "$item"
        case "$item" in
          alacritty)
            if ! (command_exists alacritty); then
              brew install --cask alacritty

              # Enable smoothing on mac
              doo defaults write -g CGFontRenderingFontSmoothingDisabled -bool NO
              doo defaults -currentHost write -globalDomain AppleFontSmoothing -int 2
            else
              installed 'alacritty'
            fi
            ;;
          android-sdk)
            # https://stackoverflow.com/questions/46402772/failed-to-install-android-sdk-java-lang-noclassdeffounderror-javax-xml-bind-a
            brew install --cask android-sdk
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
            #
            # export ANDROID_SDK_ROOT=$(brew --prefix)/share/android-sdk
            # export ANDROID_HOME=$(brew --prefix)/share/android-sdk
            # export PATH="$PATH:${ANDROID_HOME}/tools/bin"
            # export PATH="$PATH:${ANDROID_HOME}/platform-tools"
            #
            # Note: `android-sdk` has been officially discontinued upstream. We
            # need to migrate to `android-commandlinetools`.
            ;;
          android-commandlinetools)
            export ANDROID_SDK_ROOT="${BREW_HOME}/share/android-commandlinetools"
            yes | sdkmanager --licenses
            sdkmanager --list_installed
            sdkmanager --update
            sdkmanager "build-tools;33.0.0" "platform-tools" "emulator" "system-images;android-33;google_apis;x86_64" "platforms;android-33"
            sdkmanager "build-tools;33.0.0" "platform-tools" "emulator" "system-images;android-33;google_apis;arm64-v8a" "platforms;android-33"
            avdmanager create avd -n "Pixel_5" -d "pixel_5" -k "system-images;android-33;google_apis;x86_64"
            avdmanager create avd -n "Pixel_5" -d "pixel_5" -k "system-images;android-33;google_apis;arm64-v8a"
            ;;
          android-ndk)
            # Use JAVA 1.8 with JENV for the following  commands
            brew install --cask android-ndk
            yes | sdkmanager --licenses
            # Install all of the Android SDK components (you will be prompted to
            # agree to license info and then this will take a while to run):
            touch "${HOME}/.android/repositories.cfg"
            # accept the licenses
            yes | sdkmanager --licenses
            sdkmanager --update
            sdkmanager --install emulator
            sdkmanager --install platform-tools
            sdkmanager --install 'system-images;android-31;google_apis_playstore;arm64-v8a'
            sdkmanager --install 'extras;intel;Hardware_Accelerated_Execution_Manager'
            sdkmanager --install 'build-tools;30.0.1'
            sdkmanager --install 'platforms;android-31'
            sdkmanager --list

            # KITKAT (4.4) API 19
            # sdkmanager --install "system-images;android-19;google_apis;x86"

            # LOLLIPOP (5.0) API 21
            # sdkmanager --install "system-images;android-21;google_apis;x86_64"

            # LOLLIPOP (5.1) API 22
            # sdkmanager --install "system-images;android-22;google_apis;x86_64"

            # MARSHMELLOW (6.0) API 23
            # sdkmanager --install "system-images;android-23;google_apis;x86_64"

            # NOUGAT (7.0) API 24
            # sdkmanager --install "system-images;android-24;google_apis;x86_64"

            # NOUGAT (7.1) API 25
            # sdkmanager --install "system-images;android-25;google_apis;x86_64"

            # OREO (8.0) API 26
            sdkmanager --install "system-images;android-26;google_apis;x86_64"

            # OREO (8.1) API 27
            sdkmanager --install "system-images;android-27;default;x86_64"

            # PIE (9.0) API 28
            sdkmanager --install "system-images;android-28;google_apis;x86_64"

            # TEN (10.0) API 29
            sdkmanager --install "system-images;android-29;google_apis;x86_64"
            sdkmanager --install "system-images;android-29;default;x86_64"

            # Beta (11.0) API 30
            sdkmanager --install "system-images;android-30;google_apis_playstore;x86_64"

            # API 31
            sdkmanager --install "system-images;android-31;google_apis_playstore;arm64-v8a"

            # to create android virtual device
            #
            # echo "no" | avdmanager create avd --force --name "pixel_4.4" --device "pixel" --package "system-images;android-19;google_apis;x86" --tag "google_apis" --abi "x86"
            # echo "no" | avdmanager create avd --force --name "pixel_5.0" --device "pixel" --package "system-images;android-21;google_apis;x86_64" --tag "google_apis" --abi "x86_64"
            # echo "no" | avdmanager create avd --force --name "pixel_5.1" --device "pixel" --package "system-images;android-22;google_apis;x86_64" --tag "google_apis" --abi "x86_64"
            # echo "no" | avdmanager create avd --force --name "pixel_6.0" --device "pixel" --package "system-images;android-23;google_apis;x86_64" --tag "google_apis" --abi "x86_64"
            # echo "no" | avdmanager create avd --force --name "pixel_7.0" --device "pixel" --package "system-images;android-24;google_apis;x86_64" --tag "google_apis" --abi "x86_64"
            # echo "no" | avdmanager create avd --force --name "pixel_7.1" --device "pixel" --package "system-images;android-25;google_apis;x86_64" --tag "google_apis" --abi "x86_64"
            echo "no" | avdmanager create avd --force --name "pixel_8.0" --device "pixel" --package "system-images;android-26;google_apis;x86_64" --tag "google_apis" --abi "x86_64"
            echo "no" | avdmanager create avd --force --name "pixel_8.1" --device "pixel" --package "system-images;android-27;default;x86_64" --tag "default" --abi "x86_64"
            echo "no" | avdmanager create avd --force --name "pixel_9.0" --device "pixel" --package "system-images;android-28;google_apis;x86_64" --tag "google_apis" --abi "x86_64"
            echo "no" | avdmanager create avd --force --name "pixel_10.0" --device "pixel" --package "system-images;android-29;google_apis;x86_64" --tag "google_apis" --abi "x86_64"
            echo "no" | avdmanager create avd --force --name "pixel_11.0" --device "pixel" --package "system-images;android-30;google_apis_playstore;x86_64" --tag "google_apis_playstore" --abi "x86_64"
            echo "no" | avdmanager create avd --force --name "pixel_12.0" --device "pixel" --package "system-images;android-31;google_apis_playstore;x86_64" --tag "google_apis_playstore" --abi "x86_64"
            echo "no" | avdmanager create avd --force --name "pixel_12.0" --device "pixel" --package "system-images;android-31;google_apis_playstore;arm64-v8a" --tag "google_apis_playstore" --abi "arm64-v8a"

            # to create generic virtual device
            #
            # echo "no" | avdmanager create avd --force --name "generic_4.4" --package "system-images;android-19;google_apis;x86" --tag "google_apis" --abi "x86"
            # echo "no" | avdmanager create avd --force --name "generic_5.0" --package "system-images;android-21;google_apis;x86" --tag "google_apis" --abi "x86"
            # echo "no" | avdmanager create avd --force --name "generic_5.1" --package "system-images;android-22;google_apis;x86" --tag "google_apis" --abi "x86"
            # echo "no" | avdmanager create avd --force --name "generic_6.0" --package "system-images;android-23;google_apis;x86" --tag "google_apis" --abi "x86"
            # echo "no" | avdmanager create avd --force --name "generic_7.0" --package "system-images;android-24;google_apis;x86" --tag "google_apis" --abi "x86"
            # echo "no" | avdmanager create avd --force --name "generic_7.1" --package "system-images;android-25;google_apis;x86" --tag "google_apis" --abi "x86"
            echo "no" | avdmanager create avd --force --name "generic_8.0" --package "system-images;android-26;google_apis;x86_64" --tag "google_apis" --abi "x86_64"
            echo "no" | avdmanager create avd --force --name "generic_8.1" --package "system-images;android-27;default;x86_64" --tag "google_apis" --abi "x86_64"
            echo "no" | avdmanager create avd --force --name "generic_9.0" --package "system-images;android-28;google_apis;x86_64" --tag "google_apis" --abi "x86_64"
            echo "no" | avdmanager create avd --force --name "generic_10.0" --package "system-images;android-29;google_apis;x86_64" --tag "google_apis" --abi "x86_64"
            echo "no" | avdmanager create avd --force --name "generic_11.0" --package "system-images;android-30;google_apis_playstore;x86_64" --tag "google_apis_playstore" --abi "x86_64"
            echo "no" | avdmanager create avd --force --name "generic_12.0" --package "system-images;android-31;google_apis_playstore;x86_64" --tag "google_apis_playstore" --abi "x86_64"

            # add aliases
            #
            # alias pixel_8.0 ='emulator @pixel_8.0 -no-boot-anim -netdelay none -no-snapshot -wipe-data -skin 1080x1920'
            # alias pixel_8.1 ='emulator @pixel_8.1 -no-boot-anim -netdelay none -no-snapshot -wipe-data -skin 1080x1920'
            # alias pixel_9.0 ='emulator @pixel_9.0 -no-boot-anim -netdelay none -no-snapshot -wipe-data -skin 1080x1920'
            # alias pixel_9.0 ='emulator @pixel_9.0 -no-boot-anim -netdelay none -no-snapshot -wipe-data -skin 1080x1920'
            # alias pixel_10.0 ='emulator @pixel_10.0 -no-boot-anim -netdelay none -no-snapshot -wipe-data -skin 1080x1920'

            # alias generic_8.0='emulator @generic_8.0 -no-boot-anim -netdelay none -no-snapshot -wipe-data -skin 768x1280'
            # alias generic_8.1='emulator @generic_8.1 -no-boot-anim -netdelay none -no-snapshot -wipe-data -skin 768x1280'
            # alias generic_9.0='emulator @generic_9.0 -no-boot-anim -netdelay none -no-snapshot -wipe-data -skin 768x1280'

            # You can run all of the emulators above with a -read-only parameter to run multiple emulators at the same time, but this is an experimental feature right now.

            # be sure to add keyboard to all created android emulators
            # for f in ~/.android/avd/*.avd/config.ini; do echo 'hw.keyboard=yes' >> "$f"; done

            ;;
          adoptopenjdk8)
            # android-sdk requires Java 8. You can install it with:
            brew install --cask adoptopenjdk/openjdk/adoptopenjdk8
            mkdir -p "${HOME}/.jenv/versions"
            jenv add "$(/usr/libexec/java_home -v1.8)"
            # jenv global 1.8 # To unset the version `jenv global system`
            ;;
          adoptopenjdk11)
            brew install --cask adoptopenjdk/openjdk/adoptopenjdk11
            mkdir -p "${HOME}/.jenv/versions"
            jenv add "$(/usr/libexec/java_home -v11)"
            jenv global 11 # To unset the version `jenv global system`
            export JAVA_HOME=$(/usr/libexec/java_home -v1.11)
            ;;
          adoptopenjdk14)
            brew install --cask adoptopenjdk/openjdk/adoptopenjdk14
            mkdir -p "${HOME}/.jenv/versions"
            jenv add "$(/usr/libexec/java_home -v14)"
            ;;
          temurin8)
            # android-sdk requires Java 8. You can install it with:
            brew install --cask temurin8
            mkdir -p "${HOME}/.jenv/versions"
            jenv add "$(/usr/libexec/java_home -v1.8)"
            # jenv global 1.8 # To unset the version `jenv global system`
            ;;
          temurin11)
            brew install --cask temurin11
            mkdir -p "${HOME}/.jenv/versions"
            jenv add "$(/usr/libexec/java_home -v11)"
            jenv global 11.0 # To unset the version `jenv global system`
            export JAVA_HOME=$(/usr/libexec/java_home -v1.11)
            ;;
          temurin)
            # android-sdk requires Java 8. You can install it with:
            brew install --cask temurin
            mkdir -p "${HOME}/.jenv/versions"
            jenv add "$(/usr/libexec/java_home -v1.8)"
            # jenv global 1.8 # To unset the version `jenv global system`
            ;;
          zulu11)
            brew install --cask zulu11
            mkdir -p "${HOME}/.jenv/versions"
            unset JAVA_TOOL_OPTIONS
            jenv add "$(/usr/libexec/java_home -v11)"
            jenv global 11 # To unset the version `jenv global system`
            ;;
          java6)
            brew install --cask java6
            jenv add "$(/usr/libexec/java_home -v1.6)"
            ;;
          java7)
            brew install --cask java7
            jenv add "$(/usr/libexec/java_home -v1.7)"
            ;;
          java8)
            brew install --cask java8
            jenv add "$(/usr/libexec/java_home -v1.8)"
            ;;
          java9)
            brew install --cask java9
            jenv add "$(/usr/libexec/java_home -v9)"
            ;;
          kdiff3)
            brew install --cask kdiff3
            git clone git@github.com:csatf/git-tower-kdiff3-shim.git
            (cd git-tower-kdiff3-shim && ./install.sh)
            rm -rf git-tower-kdiff3-shim
            ;;
          *)
            # In some cases (like when installing `cask`) regular `emacs`
            # package will be required. In such cases you might want to install
            # all dependencies manually (except for `emacs`) and then install
            # desired package with `--ignore-dependencies` option.
            brew install --cask --force "${item}"
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

    # mas list | awk  '{ printf "%14s # %s\n", $1, $2 }'
    local __mas_apps=($(mas list | awk '{print ""$1" # "$2}'))
    local __mas_app_names=( $(mas list | awk '{print $2}') )

    local -a mas_missing_apps
    local -a mas_desired_apps=(
     '1006087419' # SnippetsLab
     '1233861775' # Acorn
     '1388020431' # DevCleaner
     '1450874784' # Transporter
     '1462643128' # TableFlip
     '1474276998' # HP
     '1543753042' # Log-Viewer
     '1579200037' # Medis
     '402383384' # Base
     '403504866' # PCalc
     '407963104' # Pixelmator
     '409183694' # Keynote
     '409201541' # Pages
     '409203825' # Numbers
     '411643860' # DaisyDisk
     '412946682' # SubnetCalc
     '413965349' # Soulver
     '429449079' # Patterns
     '439697913' # Icon
     '442160773' # Cocoa
     '444990433' # JsonParser
     '445189367' # PopClip
     '457516296' # Quick
     '459413843' # ColorStrokes
     '467939042' # Growl
     '489880259' # ScreenShot
     '494803304' # WiFi
     '520265986' # Ultra
     '533696630' # Webcam
     '536511979' # Characters
     '577085396' # Unclutter
     '581789185' # Napkin
     '593294811' # MultiMarkdown
     '640841706' # Base64Anywhere
     '669462988' # Bit
     '720669838' # iThoughtsX
     '740472063' # Focus
     '768666595' # Iconie
     '824171161' # Affinity
     '824183456' # Affinity
     '866773894' # Quiver
     '889428659' # xScope
     '896450579' # Textual
     '920404675' # Monodraw
     '931657367' # Calcbot
     '961632517' # Be
     '965645209' # Date
     '969418666' # ColorSnapper2
     '975937182' # Fantastical
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

# Here we go.. ask for the administrator password upfront and run a
# keep-alive to update existing `sudo` time stamp until script has finished
sudo -v
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

run_brew
run_mas
