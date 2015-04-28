cite 'about-alias'
about-alias 'common git abbreviations'

# Aliases
alias gaa='git add --all'
alias gff='git merge --ff-only'
alias pullff='git pull --ff-only'
alias noff='git merge --no-ff'
alias gfa='git fetch -all'
alias pom='git push origin master'
alias pod='git push origin develop'
alias gdi='git diff'
alias gdc='git diff --cached'
alias gds='git diff --stats=160,120'
alias gdh1='git diff HEAD~1'
alias gcl='git clone'
alias ga='git add'
alias gall='git add .'
alias gus='git reset HEAD'
alias gm="git merge"
alias get='git'
alias gst="git status -sb"
alias gs='git status'
alias gss='git status -s'
alias gl='git pull'
alias gpr='git pull --rebase'
alias gpp='git pull && git push'
alias gup='git fetch && git rebase'
alias gp='git push'
alias gpo='git push origin'
alias gdv='git diff -w "$@" | vim -R -'
alias gc='git commit -v'
alias gca='git commit -v -a'
alias gcm='git commit -v -m'
alias gci='git commit --interactive'
alias gcv='git commit --verbose'
alias gb='git branch'
alias gba='git branch -a'
alias gcount='git shortlog -sn'
alias gcp='git cherry-pick'
alias gco='git checkout'
alias gexport='git archive --format zip --output'
alias gdel='git branch -D'
alias gmu='git fetch origin -v; git fetch upstream -v; git merge upstream/master'
alias gll='git log --graph --pretty=oneline --abbrev-commit'
alias gg="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
alias ggs="gg --stat"
alias gsl="git shortlog -sn"
alias gw="git whatchanged"
# alias gt="git tag"
# gt: Go Top
alias gt='cd $(git rev-parse --show-toplevel 2>/dev/null || (echo "."; echo "Not within a git repository" >&2))'
alias gta="git tag -a"
alias gtd="git tag -d"
alias gtl="git tag -l"
alias gpu="git fetch origin -v; git fetch upstream -v; git merge upstream/master"
alias gfp="git format-patch --stdout -1"
alias gnr="git ls-files -o --exclude-standard | xargs rm"
# number of commits on branch.
# 1 rev-list lists revisions, and
# 2 master.. refers to "commits since current HEAD diverged from master"
alias gnum="git rev-list master.. | wc -l"
#count2 = "!git log master..yourbranch --pretty=oneline | wc -l"
alias glga="git log --abbrev-commit --date=relative --pretty=format:'%C(bold yellow)%h%Creset %s %C(bold yellow)<%an>%Creset'"
alias glgg="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative"
# Show branches, tags in git log
alias glgo="git log --oneline --decorate"
alias idiff="git diff | idiff"
# Diff by highlighting inline word changes instead of whole lines
alias wdiff="git diff --word-diff"
alias amend='git commit --amend'
# Quickly Commit / Uncommit Work-In-Progress
# By David Gageot (http://gist.github.com/492227):
alias wip="git add -A; git ls-files --deleted -z | xargs -0 git rm; git commit -m \"wip\""
alias unwip="git log -n 1 | grep -q -c wip && git reset HEAD~1"

case $OSTYPE in
  darwin*)
    alias gtls="git tag -l | gsort -V"
    ;;
  *)
    alias gtls='git tag -l | sort -V'
    ;;
esac

if [ -z "$EDITOR" ]; then
    case $OSTYPE in
      linux*)
        alias gd='git diff | vim -R -'
        ;;
      darwin*)
        alias gd='git diff | mate'
        ;;
      *)
        alias gd='git diff'
        ;;
    esac
else
    alias gd="git diff | $EDITOR"
fi
