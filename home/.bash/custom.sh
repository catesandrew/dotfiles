#######################################################################
# Custom Machine-Specific Bash Tweaks                                 #
# version 0.2.1                                                       #
# by Paul Duncan <pabs@pablotron.org>                                 #
#                                                                     #
# the basic idea for this stuff came from mazeone's bashism (and he   #
# says he got it from ricdude, hehe), but I've expanded it quite a    #
# bit...                                                              #
#                                                                     #
# The easiest way to explain this stuff is with an example.  Say      #
# you're on a Linux machine named joe.schmoe.net.  After logging in,  #
# the magic below will check for files in the following order.  If it #
# finds a matching file, it will source (execute) the file, then      #
# stop.                                                               #
#                                                                     #
# Example: (an interactive shell login on a Linux system named        #
# joe.schmoe.net) will look for files in the following order:         #
#   $HOME/.bash/custom/bashrc-joe.schmoe.net-Linux-interactive        #
#   $HOME/.bash/custom/bashrc-joe.schmoe.net-Linux                    #
#   $HOME/.bash/custom/bashrc-joe-Linux-interactive                   #
#   $HOME/.bash/custom/bashrc-joe-Linux                               #
#   $HOME/.bash/custom/bashrc-joe-interactive                         #
#   $HOME/.bash/custom/bashrc-joe                                     #
#   $HOME/.bash/custom/bashrc-Linux-interactive                       #
#   $HOME/.bash/custom/bashrc-Linux                                   #
#                                                                     #
# I'm not sure if this is the correct behavior or not; it may change  #
# in a future release once I've had a chance to test it out a bit     #
# more.                                                               #
#######################################################################
BT_CUSTOM_PREFIX="$HOME/.bash_custom"
btc_fqdn_os="$BT_CUSTOM_PREFIX/bashrc-"`hostname -f`"-"`uname`
btc_host_os="$BT_CUSTOM_PREFIX/bashrc-"`hostname`"-"`uname`
btc_host="$BT_CUSTOM_PREFIX/bashrc-"`hostname`
btc_os="$BT_CUSTOM_PREFIX/bashrc-"`uname`

for i in "$btc_fqdn_os" "$btc_host_os" "$btc_host" "$btc_os"; do
    if [ -n "$PS1" -a -f $i"-interactive" ]; then
        source "$i""-interactive"
        break
    else if [ -f "$i" ]; then
        source "$i"
        break
    fi; fi
done

