# curl https://raw.githubusercontent.com/dcreemer/1pass/master/1pass > /usr/local/bin/1pass
# chmod a+x /usr/local/bin/1pass
if hash 1pass 2>/dev/null; then
  function fuzzpass() {
    local item arg

    arg="$1"
    if [ "$arg" == "" ]; then
      arg="password"
    fi

    item="$(1pass | fzf)";
    [[ ! -z "$item" ]] && 1pass "${item}" "${arg}"
  }
fi
