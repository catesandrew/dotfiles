if [ $(uname) = "Linux" ]; then
    # https://mkaz.com/2013/01/13/ubuntu-guide-for-mac-converts/
    alias pbcopy=&#039;xclip -selection clipboard&#039;
    alias pbpaste=&#039;xclip -selection clipboard -o&#039;
    alias open=xdg-open
fi
