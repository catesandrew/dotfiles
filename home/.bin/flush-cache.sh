#!/bin/bash

if [ -f "/Library/LaunchDaemons/homebrew.mxcl.dnsmasq.plis" ]; then
  sudo launchctl unload /Library/LaunchDaemons/homebrew.mxcl.dnsmasq.plist
fi

if [ -f "/Library/LaunchDaemons/homebrew.mxcl.dnsmasq.plis" ]; then
  sudo launchctl load /Library/LaunchDaemons/homebrew.mxcl.dnsmasq.plist
fi

# dscacheutil -flushcache

sudo killall -HUP mDNSResponder
# sudo discoveryutil udnsflushcaches
# sudo discoveryutil mdnsflushcache
# sudo discoveryutil mdnsrestartregistrations
