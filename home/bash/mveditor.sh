#!/usr/bin/env bash

# Abstract:
#     Script for using MacVim as the default EDITOR.
#
# Author:
#     Ben Spaulding <http://benspaulding.us/>
#
# Details:
#     This script allows MacVim to be the ``EDITOR`` in a more robust way than
#     the ``mvim`` command line tool allows. This is because the ``mvim``
#     command exits immediately after the file arguments are opened, unless the
#     ``-f`` switch is used. However, some tools, such as crontab, fail with a
#     multiple-term ``EDITOR``. This script can be set as the ``EDITOR``,
#     resolving that issue. The ``-c "au VimLeave * maca hide:"`` switch and
#     argument are also used, which returns focus to the referring app (e.g.,
#     Terminal.app) after editing is complete.
#
# Usage:
#     Save this script to somewhere like your ~/bin directory. You can then put
#     something such as the following in your ~/.bashrc file::
#
#         # Set mveditor/MacVim as EDITOR.
#         if [ -f "$HOME/bin/mveditor" ]; then
#             export EDITOR="$HOME/bin/mveditor"
#         else
#             echo "WARNING: Can't find mveditor. Using vim instead."
#             export EDITOR="vim"
#         fi
#
if [ -f "`which mvim`" ]; then
    mvim -f -c "au VimLeave * maca hide:" "$@"
else
    echo "ERROR: Can't find mvim. Did you install the MacVim utility?"
    echo "NOTICE: Using vim instead."
    vim "$@"
fi
