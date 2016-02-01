#!/bin/bash
# composure - by erichs
# light-hearted functions for intuitive shell programming

# version: 1.2.1
# latest source available at http://git.io/composure

# install: source this script in your ~/.profile or ~/.${SHELL}rc script
# known to work on bash, zsh, and ksh93

# 'plumbing' functions

_composure_keywords ()
{
  echo "about author example group param version"
}

_generate_metadata_functions() {
  typeset f
  for f in $(_composure_keywords)
  do
    eval "$f() { :; }"
  done
}

# 'porcelain' functions

cite ()
{
  about 'creates one or more meta keywords for use in your functions'
  param 'one or more keywords'
  example '$ cite url username'
  example '$ url http://somewhere.com'
  example '$ username alice'
  group 'composure'

  # this is the storage half of the 'metadata' system:
  # we create dynamic metadata keywords with function wrappers around
  # the NOP command, ':'

  # anything following a keyword will get parsed as a positional
  # parameter, but stay resident in the ENV. As opposed to shell
  # comments, '#', which do not get parsed and are not available
  # at runtime.

  # a BIG caveat--your metadata must be roughly parsable: do not use
  # contractions, and consider single or double quoting if it contains
  # non-alphanumeric characters

  if [ -z "$1" ]; then
    printf '%s\n' 'missing parameter(s)'
    return
  fi

  typeset keyword
  for keyword in "$@"; do
    eval "$keyword() { :; }"
  done
}

_generate_metadata_functions

: <<EOF
License: The MIT License

Copyright © 2012, 2013 Erich Smith

Permission is hereby granted, free of charge, to any person obtaining a copy of this
software and associated documentation files (the "Software"), to deal in the Software
without restriction, including without limitation the rights to use, copy, modify,
merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be included in all copies
or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
EOF
