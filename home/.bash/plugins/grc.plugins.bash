# load grc, if you are using it

if brew_contains_element "grc"; then
  # . "${__dot_brew_home}/etc/grc.bashrc"
  GRC=`which grc`
  if [ "$TERM" != dumb  ] && [ -n "$GRC"  ]
  then
      alias colourify="$GRC -es --colour=auto"
      alias configure='colourify ./configure'
      alias diff='colourify diff'
      alias make='colourify make'
      alias gcc='colourify gcc'
      alias g++='colourify g++'
      alias as='colourify as'
      alias gas='colourify gas'
      alias ld='colourify ld'
      alias netstat='colourify netstat'
      alias ping='colourify ping'
      alias traceroute='colourify /usr/sbin/traceroute'
      alias dig='colourify dig'
      alias mount='colourify mount'
      alias ps='colourify ps'
      alias mtr='colourify mtr'

      alias df='colourify df'
      alias head='colourify head'
      alias tail='colourify tail'

      if [ "${__dot_system_type}" == "Darwin" ]; then
          if brew_contains_element "coreutils"; then
              alias df='colourify gdf'
              alias head='colourify ghead'
              alias tail='colourify gtail'
          fi
      fi
  fi
fi





