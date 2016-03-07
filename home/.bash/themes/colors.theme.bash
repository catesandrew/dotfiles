function __ {
  echo "$@"
}

function __make_ansi {
  next=$1; shift
  echo "\[\e[$("__$next" "$@")m\]"
}

function __make_echo {
  next=$1; shift
  echo "\033[$("__$next" "$@")m"
}

function __reset {
  next=$1; shift
  out="$("__$next" "$@")"
  echo "0${out:+;${out}}"
}

function __bold {
  next=$1; shift
  out=$(__"$next" "$@")
  echo "${out:+${out};}1"
}

function __faint {
  next=$1; shift
  out=$("__$next" "$@")
  echo "${out:+${out};}2"
}

function __italic {
  next=$1; shift
  out=$("__$next" "$@")
  echo "${out:+${out};}3"
}

function __underline {
  next=$1; shift
  out=$("__$next" "$@")
  echo "${out:+${out};}4"
}

function __negative {
  next=$1; shift
  out=$("__$next" "$@")
  echo "${out:+${out};}7"
}

function __crossed {
  next=$1; shift
  out=$("__$next" "$@")
  echo "${out:+${out};}8"
}

function __color_normal_fg {
  echo "3$1"
}

function __color_normal_bg {
  echo "4$1"
}

function __color_bright_fg {
  echo "9$1"
}

function __color_bright_bg {
  echo "10$1"
}

function __color_black   {
  echo "0"
}

function __color_red   {
  echo "1"
}

function __color_green   {
  echo "2"
}

function __color_yellow  {
  echo "3"
}

function __color_blue  {
  echo "4"
}

function __color_magenta {
  echo "5"
}

function __color_cyan  {
  echo "6"
}

function __color_white   {
  echo "7"
}

function __color_rgb {
  local r=$1 && local g=$2 && local b=$3
  [[ $r = "$g" && $g = "$b" ]] && echo $(( r / 11 + 232 )) && return # gray range above 232
  echo "8;5;$(( (r * 36  + b * 6 + g) / 51 + 16 ))"
}

function __color {
  color=$1; shift
  case "$1" in
    fg|bg) side="$1"; shift ;;
    *) side=fg;;
  esac
  case "$1" in
    normal|bright) mode="$1"; shift;;
    *) mode=normal;;
  esac
  [[ $color == "rgb" ]] && rgb="$1 $2 $3"; shift 3

  next=$1; shift
  out=$("__$next" "$@")
  echo "$("__color_${mode}_${side}" "$("__color_${color}" "${rgb}")")"${out:+;${out}}
}

function __black {
  __color black "$@"
}

function __red {
  __color red "$@"
}

function __green {
  __color green "$@"
}

function __yellow {
  __color yellow "$@"
}

function __blue {
  __color blue "$@"
}

function __magenta {
  __color magenta "$@"
}

function __cyan {
  __color cyan "$@"
}

function __white {
  __color white "$@"
}

function __rgb {
  __color rgb "$@"
}

function __color_parse {
  next=$1; shift
  "__$next" "$@"
}

function color {
  __color_parse make_ansi "$@"
}

function echo_color {
  __color_parse make_echo "$@"
}

black=$(color black)
export black
red=$(color red)
export red
green=$(color green)
export green
yellow=$(color yellow)
export yellow
blue=$(color blue)
export blue
purple=$(color magenta)
export purple
cyan=$(color cyan)
export cyan
white=$(color white bold)
export white
orange=$(color red fg bright)
export orange

bold_black=$(color black bold)
export bold_black
bold_red=$(color red bold)
export bold_red
bold_green=$(color green bold)
export bold_green
bold_yellow=$(color yellow bold)
export bold_yellow
bold_blue=$(color blue bold)
export bold_blue
bold_purple=$(color magenta bold)
export bold_purple
bold_cyan=$(color cyan bold)
export bold_cyan
bold_white=$(color white bold)
export bold_white
bold_orange=$(color red fg bright bold)
export bold_orange

underline_black=$(color black underline)
export underline_black
underline_red=$(color red underline)
export underline_red
underline_green=$(color green underline)
export underline_green
underline_yellow=$(color yellow underline)
export underline_yellow
underline_blue=$(color blue underline)
export underline_blue
underline_purple=$(color magenta underline)
export underline_purple
underline_cyan=$(color cyan underline)
export underline_cyan
underline_white=$(color white underline)
export underline_white
underline_orange=$(color red fg bright underline)
export underline_orange

background_black=$(color black bg)
export background_black
background_red=$(color red bg)
export background_red
background_green=$(color green bg)
export background_green
background_yellow=$(color yellow bg)
export background_yellow
background_blue=$(color blue bg)
export background_blue
background_purple=$(color magenta bg)
export background_purple
background_cyan=$(color cyan bg)
export background_cyan
background_white=$(color white bg bold)
export background_white
background_orange=$(color red bg bright)
export background_orange

normal=$(color reset)
export normal
reset_color=$(__make_ansi '' 39)
export reset_color

# These colors are meant to be used with `echo -e`
# echo_black=$(echo_color reset black)
# export echo_black
# echo_red=$(echo_color reset red)
# export echo_red
# echo_green=$(echo_color reset green)
# export echo_green
# echo_yellow=$(echo_color reset yellow)
# export echo_yellow
# echo_blue=$(echo_color reset blue)
# export echo_blue
# echo_purple=$(echo_color reset magenta)
# export echo_purple
# echo_cyan=$(echo_color reset cyan)
# export echo_cyan
# echo_white=$(echo_color reset white bold)
# export echo_white
# echo_orange=$(echo_color reset red fg bright)
# export echo_orange

# echo_bold_black=$(echo_color black bold)
# export echo_bold_black
# echo_bold_red=$(echo_color red bold)
# export echo_bold_red
# echo_bold_green=$(echo_color green bold)
# export echo_bold_green
# echo_bold_yellow=$(echo_color yellow bold)
# export echo_bold_yellow
# echo_bold_blue=$(echo_color blue bold)
# export echo_bold_blue
# echo_bold_purple=$(echo_color magenta bold)
# export echo_bold_purple
# echo_bold_cyan=$(echo_color cyan bold)
# export echo_bold_cyan
# echo_bold_white=$(echo_color white bold)
# export echo_bold_white
# echo_bold_orange=$(echo_color red fg bright bold)
# export echo_bold_orange

# echo_underline_black=$(echo_color black underline)
# export echo_underline_black
# echo_underline_red=$(echo_color red underline)
# export echo_underline_red
# echo_underline_green=$(echo_color green underline)
# export echo_underline_green
# echo_underline_yellow=$(echo_color yellow underline)
# export echo_underline_yellow
# echo_underline_blue=$(echo_color blue underline)
# export echo_underline_blue
# echo_underline_purple=$(echo_color magenta underline)
# export echo_underline_purple
# echo_underline_cyan=$(echo_color cyan underline)
# export echo_underline_cyan
# echo_underline_white=$(echo_color white underline)
# export echo_underline_white
# echo_underline_orange=$(echo_color red fg bright underline)
# export echo_underline_orange

# echo_background_black=$(echo_color black bg)
# export echo_background_black
# echo_background_red=$(echo_color red bg)
# export echo_background_red
# echo_background_green=$(echo_color green bg)
# export echo_background_green
# echo_background_yellow=$(echo_color yellow bg)
# export echo_background_yellow
# echo_background_blue=$(echo_color blue bg)
# export echo_background_blue
# echo_background_purple=$(echo_color magenta bg)
# export echo_background_purple
# echo_background_cyan=$(echo_color cyan bg)
# export echo_background_cyan
# echo_background_white=$(echo_color white bg bold)
# export echo_background_white
# echo_background_orange=$(echo_color red bg bright)
# export echo_background_orange

# echo_normal=$(echo_color reset)
# export echo_normal
# echo_reset_color=$(__make_echo '' 39)
# export echo_reset_color
