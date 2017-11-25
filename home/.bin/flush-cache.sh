#!/bin/bash

sudo launchctl unload /Library/LaunchDaemons/homebrew.mxcl.dnsmasq.plist
sudo launchctl load /Library/LaunchDaemons/homebrew.mxcl.dnsmasq.plist

# dscacheutil -flushcache

sudo killall -HUP mDNSResponder
# sudo discoveryutil udnsflushcaches
# sudo discoveryutil mdnsflushcache
# sudo discoveryutil mdnsrestartregistrations
