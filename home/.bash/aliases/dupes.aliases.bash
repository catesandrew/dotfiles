# Some aliases for Homebrew Dupes
if [ "${__dot_system_type}" == "Darwin" ]; then
  if brew_contains_element "grep"; then
    alias grep='ggrep --color=always'
    alias grepno="ggrep --color=never -n -E '.*'"
    alias egrep='gegrep --color=always'
    alias fgrep='gfgrep --color=auto'
  fi

  if brew_contains_element "gnu-time"; then
    alias time='gtime'
  fi

  if brew_contains_element "gnu-which"; then
    alias which='gwhich'
  fi

  if brew_contains_element "findutils"; then
    alias find='gfind'
    alias locate='glocate'
    alias updatedb='gupdatedb'
    alias xargs='gxargs'
  fi

  if brew_contains_element "gnu-sed"; then
    alias sed='gsed'
  fi

  if brew_contains_element "coreutils"; then
    export COREUTILS_HOME="${__dot_brew_home}/opt/coreutils"
    export MANPATH="${COREUTILS_HOME}/libexec/gnuman:$MANPATH"

    if [ -f "${HOME}/.dircolors" ]; then
        eval $(gdircolors -b "${HOME}/.dircolors")
        alias ls="gls -G --color=always"
    else
      alias ls='gls'
    fi

    if brew_contains_element "ccat"; then
      alias cat='ccat'
    else
      alias cat='gcat'
    fi

    ## Directory
    alias rd='grmdir'

    alias base32='gbase32'
    alias base64='gbase64'
    alias basename='gbasename'
    alias chcon='gchcon'
    # alias chgrp='gchgrp'
    alias chgrp='gchgrp --preserve-root'
    # alias chmod='gchmod'
    alias chmod='gchmod --preserve-root'
    # alias chown='gchown'
    alias chown='gchown --preserve-root'
    alias chroot='gchroot'
    alias cksum='gcksum'
    alias comm='gcomm'
    alias cp='gcp'
    # alias cp='gcp -i'
    alias csplit='gcsplit'
    alias cut='gcut'
    alias date='gdate'
    alias dd='gdd'
    alias df='gdf'
    alias dir='gdir'
    alias dircolors='gdircolors'
    alias dirname='gdirname'
    alias du='gdu'
    alias echo='gecho'
    alias env='genv'
    alias expand='gexpand'
    alias expr='gexpr'
    alias factor='gfactor'
    alias false='gfalse'
    alias fmt='gfmt'
    alias fold='gfold'
    alias groups='ggroups'
    alias head='ghead'
    alias hostid='ghostid'
    alias id='gid'
    alias install='ginstall'
    alias join='gjoin'
    alias kill='gkill'
    alias link='glink'
    alias ln='gln'
    # alias ln='gln -i'
    alias logname='glogname'
    alias md5sum='gmd5sum'
    # alias mkdir='gmkdir'
    alias mkdir='gmkdir -pv'         # Create parent directories on demand
    alias mkfifo='gmkfifo'
    alias mknod='gmknod'
    alias mktemp='gmktemp'
    alias mv='gmv'
    # alias mv='gmv -i'
    alias nice='gnice'
    alias nl='gnl'
    alias nohup='gnohup'
    alias nproc='gnproc'
    alias numfmt='gnumfmt'
    alias od='god'
    alias paste='gpaste'
    alias pathchk='gpathchk'
    alias pinky='gpinky'
    alias pr='gpr'
    alias printenv='gprintenv'
    # alias printf='gprintf'
    alias ptx='gptx'
    alias pwd='gpwd'
    alias readlink='greadlink'
    alias realpath='grealpath'
    # alias rm='grm'
    # do not delete / or prompt if deleting more than 3 files at a time
    alias rm='grm -I'
    alias rmdir='grmdir'
    alias runcon='gruncon'
    alias seq='gseq'
    alias sha1sum='gsha1sum'
    alias sha224sum='gsha224sum'
    alias sha256sum='gsha256sum'
    alias sha384sum='gsha384sum'
    alias sha512sum='gsha512sum'
    alias shred='gshred'
    alias shuf='gshuf'
    alias sleep='gsleep'
    alias sort='gsort'
    alias split='gsplit'
    alias stat='gstat'
    alias stdbuf='gstdbuf'
    alias stty='gstty'
    alias sum='gsum'
    alias sync='gsync'
    alias tac='gtac'
    alias tail='gtail'
    alias tee='gtee'
    alias test='gtest'
    alias timeout='gtimeout'
    alias touch='gtouch'
    alias tr='gtr'
    alias true='gtrue'
    alias truncate='gtruncate'
    alias tsort='gtsort'
    alias tty='gtty'
    alias uname='guname'
    alias unexpand='gunexpand'
    alias uniq='guniq'
    alias unlink='gunlink'
    alias uptime='guptime'
    alias users='gusers'
    alias vdir='gvdir'
    # alias wc='gwc' # throws `not a git repo` errors
    alias who='gwho'
    alias whoami='gwhoami'
    alias yes='gyes'

  else
    # colored grep
    export GREP_COLOR='1;33'

    # colored ls
    export CLICOLOR=1
    export LSCOLORS='Gxfxcxdxdxegedabagacad'

    # compact view
    alias ls='ls -G'
  fi
elif [ "${__dot_system_type}" == "Linux" ]; then
  if [ -f "${HOME}/.dircolors" ]; then
    eval $(dircolors -b "${HOME}/.dircolors")
    alias ls="ls -G --color=always"
    alias egrep="egrep --color=always"
    alias grep="grep --color=always"
    alias grepno="grep --color=never -n -E '.*'"
  fi
fi
