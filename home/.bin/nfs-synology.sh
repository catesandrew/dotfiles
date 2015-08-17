#!/bin/bash
# Copyright (c) 2000-2014 Synology Inc. All rights reserved.

# LDAP server URI, must starts with "ldap://" or "ldaps://".
LDAP_URI=$(eval "echo $2")    # "ldaps://ldap.example.com"
# Search base of LDAP server.
BASE_DN=$(eval "echo $3")     # "dc=ldap,dc=example,dc=com"
# "yes" or "no" means use TLS for LDAP connections or not.
USE_TLS=$(eval "echo $4")
# NFS server, default use LDAP_URI.
NFS_SERVER=$(eval "echo $5")   # "nfs.example.com"
# User-defined domain name (login suffix).
DOMAIN_NAME=$(eval "echo $6")  # "example.com"
# NFS folder for home, default use /var/services/homes.
# NFS_FOLDER=$(eval "echo $7")  # "/var/services/homes"

ToUpper() {  ### <string>
   echo "$1" | tr [:lower:] [:upper:]
}

ResolveDomainName() {  ### <base_dn>
   local token= domain=

   for token in ${1//,/ }; do
       domain="$domain.${token#*=}"
   done
   echo "`ToUpper ${domain:1}`"
}

key="$1"
# key=`id -u -n`
opt="-LLLxH"
if [ "$USE_TLS" = "yes" ]; then
   opt="-Z $opt"
fi
if [ -z "$key" ]; then
   exit 0
fi


if [ -z "$NFS_SERVER" ]; then
   # Use pre-defined NFS server if any.
   NFS_SERVER="${LDAP_URI#ldap*://}"
fi
if [ -n "$NFS_FOLDER" ]; then
   # Use pre-defined NFS folder if any.
   # echo "-fstype=nfs,tcp,intr $NFS_SERVER:${NFS_FOLDER// /\\ }/${key// /\\ }"
   echo "rw|$NFS_SERVER|${NFS_FOLDER// /\\ }/${key// /\\ }|/home/shared|"
   exit 0
fi

if [ -z "$LDAP_URI" -o -z "$BASE_DN" ]; then
   >&2 echo "empty LDAP_URI or BASE_DN, please check your '$0'"
   exit 1
fi

if [ -z "$DOMAIN_NAME" ]; then
   domain="`ResolveDomainName $BASE_DN`"
else
   # Use pre-defined domain name (login suffix) if any.
   domain="`ToUpper $DOMAIN_NAME`"
fi
uid="`ldapsearch $opt "$LDAP_URI" -b "$BASE_DN" "uid=$key" uidNumber 2>/dev/null | grep 'uidNumber:' | awk '{print $2}'`"
if [ -z "$uid" ]; then
   >&2 echo "no uid found for user '$key'"
   exit 1
fi
if ! [[ "$uid" =~ ^[0-9]+ ]]; then
   >&2 echo "invalid uid '$uid'"
   exit 1
fi
num=$(( $uid >> 14 ))

# echo "-fstype=nfs,intr,hard,tcp $NFS_SERVER:/var/services/homes/@LH-$domain/$num/${key// /\\ }-$uid"
echo "rw|$NFS_SERVER|/var/services/homes/@LH-$domain/$num/${key// /\\ }-$uid|/home/shared|"

