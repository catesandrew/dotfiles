#!/bin/bash
# Tunnelling privilege ports requires sudo. However, when you run `sudo ssh` it
# uses the ~.ssh/config file from the root user. No problem, pass `-F
# ~/.ssh/config` and it works. Further expanding on the solution, pass `-E` with
# sudo like this `sudo -E` so the environment variables get copied over too.
# However, since the `~/.ssh/config` file has paths with tildes in them, the
# tilde expansion always expands to the root users home directory. It does not
# matter if the `HOME` is set to your other user, it does not matter if you have
# `always_set_home` or `set_home` or any other feature in sudoers set, no matter
# if you have HOME correct or not, the ssh program always tilde expands to the
# root users home directory.
#
# None of these work
#
# ❯ sudo -E ssh -F ~/.ssh/config -L 10443:localhost:8443 -L 902:localhost:902 andrew.cates.io
# no such identity: /var/root/.ssh/id_andrew@cates.io: No such file or directory
#
# ❯ sudo su bash -c "ssh -F ~/.ssh/config -L 10443:localhost:8443 -L 902:localhost:902 andrew.cates.io"
#
# Attempt to go to your home but with root access, doesn't work
# ❯ sudo -i
#
# An attempt to replace ~ with home directory
# ❯ ssh -F <(sed -e s/~/\\/Users\\/andrew/g .ssh/config) andrew.cates.io
#
# Now just run the following for example to forward vmware 902 ports
# sudo-ssh -L 10443:localhost:8443 -L 902:localhost:902 andrew.cates.io

TFILE=`mktemp -t delicate-resonance`
trap 'rm -f ${TFILE}' 0 1 2 3 15

sed -e "s,~,$HOME,g" ~/.ssh/config > "$TFILE"

# sudo -E ssh -F "$TFILE" -L 10443:localhost:8443 -L 902:localhost:902 andrew.cates.io
sudo -E ssh -F "$TFILE" "$@"

## trap Deletes TFILE on Exit
