#!/bin/bash

BASH_NO=no . /etc/profile

echo "<div class=\"markdown-body\">$(/usr/local/bin/grip --no-inline --norefresh --pass=${GITHUB_TOKEN} "${MARKED_PATH}" --export - |  cheerio - -s '.markdown-body')</div>"

