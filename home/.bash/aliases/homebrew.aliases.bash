# Some aliases for Homebrew

alias bup='brew update && brew upgrade'
alias bupc='brew update && brew upgrade && brew cleanup'

# TODO: update to check if cask is installed (Linux)
if [ "$(uname)" = "Darwin" ]; then
  alias bup='brew update && brew upgrade && brew cleanup --prune=1 && brew cask cleanup'
elif [ "$(uname)" = "Linux" ]; then
  alias bup='brew update && brew upgrade && brew cleanup --prune=1'
fi

alias bout='brew outdated'
alias bi='brew install'
alias brm='brew uninstall'
alias bls='brew list'
alias bs='brew search'
alias bn='brew info'
alias bdr='brew doctor'
