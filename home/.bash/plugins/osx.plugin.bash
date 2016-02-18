#!/bin/bash
if [ "$__dot_system_type" == "Darwin" ]; then

# renames the current os x terminal tab title
function tabname {
  printf "\e]1;$1\a"
}

# renames the current os x terminal window title
function winname {
  printf "\e]2;$1\a"
}

# switch dock between 2d and 3d.
# param '1: "2d" or "3d"'
# example '$ dock-switch 2d'
function dock-switch() {
  if [ $1 = 3d ] ; then
    defaults write com.apple.dock no-glass -boolean NO
    killall Dock

  elif [ $1 = 2d ] ; then
    defaults write com.apple.dock no-glass -boolean YES
    killall Dock

  else
    echo "usage:"
    echo "dock-switch 2d"
    echo "dock-switch 3d."
  fi
}

# Download a file and open it in Preview
#
function prevcurl() {
  curl "$*" | open -fa "Preview"
}

function wifi() {
  if [ -z "$CURRENT_DEVICE" ]; then
    CURRENT_DEVICE=$(networksetup -listallhardwareports | awk '$3=="Wi-Fi" {getline; print $2}')
    export CURRENT_DEVICE
  fi

  case "$#" in
    0)

      echo >&2 "Usage: wifi [help|on|off]"
      ;;
    *)
      cmd="$1"
      shift

      case "$cmd" in
        on)
          networksetup -setairportpower "${CURRENT_DEVICE}" on
          ;;
        off)
          networksetup -setairportpower "${CURRENT_DEVICE}" off
          ;;
        *)
          echo >&2 "Usage: wifi [help|on|off]"
          ;;
      esac
  esac

}

# brew install dnsmasq
if brew_contains_element "dnsmasq"; then
  function daemon-dnsmasq() {
    case "$#" in
      0)
        echo >&2 "Usage: daemon-dnsmasq [restart|on|off]"
        ;;
      *)
        cmd="$1"
        shift

        case "$cmd" in
          restart)
            sudo launchctl unload /Library/LaunchDaemons/homebrew.mxcl.dnsmasq.plist
            sudo launchctl load -w /Library/LaunchDaemons/homebrew.mxcl.dnsmasq.plist
            ;;
          on)
            sudo launchctl load -w /Library/LaunchDaemons/homebrew.mxcl.dnsmasq.plist
            ;;
          off)
            sudo launchctl unload /Library/LaunchDaemons/homebrew.mxcl.dnsmasq.plist
            ;;
          *)
            echo >&2 "Usage: daemon-dnsmasq [restart|on|off]"
            ;;
        esac
    esac
  }
fi

# brew install openconnect vpnc
# brew cask install tuntap
#
# NOTE: Add your localhost if you are using `dnsmasq`
# NOTE: The path to vpnc-script is from `vpnc`
# NOTE: http://blog.macromates.com/2006/keychain-access-from-shell/
# ~/.openconnect template follows
#
# user=username
# no-cert-check
# script INTERNAL_IP4_DNS="127.0.0.1" /usr/local/etc/vpnc/vpnc-script
# background
# passwd-on-stdin

if brew_contains_element "openconnect"; then
  function vpn() {
    case "$#" in
      0)
        echo >&2 "Usage: vpn [on|off]"
        ;;
      *)
        cmd="$1"
        shift

        if [ ! -d "${HOME}/.pids" ]; then
          mkdir -p "${HOME}/.pids"
        fi
        local __pidfile_oc="${HOME}/.pids/openconnect.pid"

        case "$cmd" in
          on)
            if [ -z "$OPENCONNECT_URL" ]; then
              echo >&2 "Error: missing environment variable OPENCONNECT_URL"
            else
              vpn destroy
              local __vpn_pass
              __vpn_pass=$(security find-generic-password -a openconnect-vpn -w)
              echo -n "${__vpn_pass}" | sudo openconnect --config "${HOME}/.openconnect" --pid-file="${__pidfile_oc}" "${OPENCONNECT_URL}"
            fi
            ;;
          off)
            if [ -z "$OPENCONNECT_URL" ]; then
              echo >&2 "Error: missing environment variable OPENCONNECT_URL"
            else
              if [ -f "${__pidfile_oc}" ]; then
                wifi off
                local __pid
                __pid=$(cat "${__pidfile_oc}")
                sudo kill -2 "$__pid"
                sudo kill "$__pid"
                if [ -f "${__pidfile_oc}" ]; then
                  rm -f "${__pidfile_oc}"
                fi
                wifi on
                sleep 4s
              fi
              daemon-dnsmasq restart
            fi
            ;;
          *)
            echo >&2 "Usage: vpn [on|off]"
            ;;
        esac
    esac
  }
fi

function get-icon() {
  APP=`echo $1|sed -e 's/\.app$//'`
  APPDIR=''
  for dir in "/Applications/" "/Applications/Utilities/"; do
    if [[ -d ${dir}$APP.app ]]; then
      APPDIR=$dir
      break
    fi
  done
  if [[ $APPDIR == '' ]]; then
    echo "App not found"
  else
    ICON=`defaults read "${APPDIR}$APP.app/Contents/Info" CFBundleIconFile|sed -e 's/\.icns$//'`
    OUTFILE="$HOME/Downloads/${APP}_icon.jpg"
    MAXAVAIL=`sips -g pixelWidth "${APPDIR}$APP.app/Contents/Resources/$ICON.icns"|tail -1|awk '{print $2}'`
    echo -n "Enter max pixel width ($MAXAVAIL): "
    read MAX
    if [[ $MAX == ''  || $MAX -gt $MAXAVAIL ]]; then
      MAX=$MAXAVAIL
    fi
    /usr/bin/sips -s format jpeg --resampleHeightWidthMax $MAX "${APPDIR}$APP.app/Contents/Resources/$ICON.icns" --out "$OUTFILE" > /dev/null 2>&1
    echo "Wrote JPEG to $OUTFILE."
    echo -n 'Open in Preview? (y/N): '
    read ANSWER
    if [[ $ANSWER == 'y' ]]; then
      open -a "Preview.app" "$OUTFILE"
    fi
  fi
}

fi
