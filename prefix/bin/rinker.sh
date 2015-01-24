#!/bin/bash

#################################################################
## ChangeIP.com bash update script                             ##
#################################################################
## Written 3/18/09 by Tom Rinker, released to the Public Domain##
#################################################################
## This is a simple bash script to preform a dDNS update with  ##
## ChangeIP.com. It uses only bash and wget, and so should be  ##
## compatible with virtually any UNIX/Linux based system with  ##
## bash. It is intended to be executed as a cron job, and      ##
## will only execute an update of dDNS records when the IP     ##
## address changes. As ChangeIP.com dDNS records have a 5 min  ##
## Time-To-Live, it is basically pointless and wasteful to     ##
## execute it more often than every 5 minutes. This script     ##
## supports logging all activity, in 3 available log levels,   ##
## and supports simple management of log file size.            ##
#################################################################
## To use this script:                                         ##
## 1) set the variables in the script below                    ##
## 2) execute the script as a cron job                         ##
#################################################################
## WARNING: This script has two potential security holes.      ##
## First, the username and password are stored plaintext in    ##
## the script, so a system user who has read access to the     ##
## script could read them. This risk can be mitigated with     ##
## careful use of file permissions on the script.              ##
## Second, the username and password will show briefly to other##
## users of the system via ps, w, or top. This risk can be     ##
## mitigated by moving the username and password to .wgetrc    ##
## This level of security is acceptable for some installations ##
## including my own, but may be unacceptable for some users.   ##
#################################################################

################ Script Variables ###############################
IPPATH=${HOME}/Library/Logs/IP                    # IP address storage file
TMPIP=/tmp/tmpIP                      # Temp IP storage file
LOGPATH=${HOME}/Library/Logs/changeip.log         # Log file
TEMP=/tmp/temp                        # Temp storage file
CIPUSER=                              # ChangeIP.com Username
CIPPASS=                              # ChangeIP.com Password
CIPSET=1                              # ChangeIP.com recordset
LOGLEVEL=2                            # 0=off,1=normal,2=verbose
LOGMAX=500                            # Max log lines, 0=unlimited
#################################################################

# get current IP from ip.changeip.com, and store in $TEMP
wget -q -U "rinker.sh wget 1.0" -O $TEMP ip.changeip.com

# parse $TEMP for the ip, and store in $TMPIP
grep IPADDR < $TEMP | cut -d= -s -f2 | cut -d- -s -f1 > $TMPIP

# compare $IPPATH with $TMPIP, and if different, execute update
if diff $IPPATH $TMPIP > /dev/null
  then                                # same IP, no update
      if [ $LOGLEVEL -eq 2 ]
        then                          # if verbose, log no change
          echo "--------------------------------" >> $LOGPATH
          date >> $LOGPATH
          echo "No Change" >> $LOGPATH
          echo -e "IP: \c" >> $LOGPATH
          cat $IPPATH >> $LOGPATH
      fi
  else                                # different IP, execute update
      wget -q -U "rinker.sh wget 1.0" -O $TEMP --http-user=$CIPUSER --http-password=$CIPPASS "https://nic.changeip.com/nic/update?cmd=update&set=$CIPSET"
      if [ $LOGLEVEL -ne 0 ]
        then                          # if logging, log update
          echo "--------------------------------" >> $LOGPATH
          date >> $LOGPATH
          echo "Updating" >> $LOGPATH
          echo -e "NewIP: \c" >> $LOGPATH
          cat $TMPIP >> $LOGPATH
          if [ $LOGLEVEL -eq 2 ]
            then                      # verbose logging
              echo -e "OldIP: \c" >> $LOGPATH
              cat $IPPATH >> $LOGPATH
              cat $TEMP >> $LOGPATH   # log the ChangeIP.com update reply
          fi
      fi
      cp $TMPIP $IPPATH               # Store new IP
fi

# if $LOGMAX not equal to 0, reduce log size to last $LOGMAX number of lines
if [ $LOGMAX -ne 0 ]
  then
      tail -n $LOGMAX $LOGPATH > $TEMP
      cp $TEMP $LOGPATH
fi
