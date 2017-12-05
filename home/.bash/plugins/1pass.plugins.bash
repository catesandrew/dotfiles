# curl https://raw.githubusercontent.com/dcreemer/1pass/master/1pass > /usr/local/bin/1pass
# chmod a+x /usr/local/bin/1pass
function fuzzpass() {
  echo "b"
  local item arg

  arg="$1"
  if [ "$arg" == "" ]; then
    arg="password"
  fi

  item="$(1pass | fzf)";
  [[ ! -z "$item" ]] && 1pass "${item}" "${arg}"
}
