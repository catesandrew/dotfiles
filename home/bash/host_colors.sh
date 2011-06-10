#######################################################################
# ~/.bash/host_colors.sh                                              #
# version 0.2.1                                                       #
# by Paul Duncan <pabs@pablotron.org>                                 #
#######################################################################

case "$HOSTNAME" in

###################
# My Workstations #
###################
  macbookpro|bobby)
  C_HOST="$CYAN"
  ;;

###########
# Servers #
###########
  www|gw-uunet|cable|ns|phoenix|debian)
  C_HOST="$GREEN"
  ;;

################
# Work-Related #
################
  black|white|tsfsrv|happy|junior|vader|maul|blue|red)
  C_HOST="$LIGHT_BLUE"
  ;;

#####################
# Friends' Machines #
#####################
  bma|offended|ws|fw|snowman|h38n1fls32o1007)
  C_HOST="$LIGHT_CYAN"
  ;;

##############
# Misc Boxes #
##############
  nerdland)
  C_HOST="$BROWN"
  ;;

esac
