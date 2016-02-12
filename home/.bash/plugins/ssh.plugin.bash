# ssh helper functions

# add entry to ssh config
# param '1: host'
# param '2: hostname'
# param '3: user'
function ssh-add() {
  echo -en "\n\nHost $1\n  HostName $2\n  User $3\n  ServerAliveInterval 30\n  ServerAliveCountMax 120" >> ~/.ssh/config
}

# list hosts defined in ssh config
function ssh-list() {
  awk '$1 ~ /Host$/ { print $2 }' ~/.ssh/config
}
