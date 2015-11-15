cite 'about-alias'
about-alias 'Tmux terminal multiplexer'

alias txl='tmux ls'
# alias txl='tmux list-sessions'
# alias txl="tmux list-sessions"

alias txn='tmux new -s'
alias txa='tmux a -t'
# alias txa='tmux attach'
# alias txa="tmux attach-session"

alias txc="clear && tmux clear-history"
alias txk="tmux kill-session -t"
