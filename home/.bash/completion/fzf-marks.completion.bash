# fzf-marks
#
# This tiny script, inspired by
# [zshmarks](https://github.com/jocelynmallon/zshmarks), can be used to create,
# delete, and navigate marks in Bash and Zsh. It is based on the command-line
# fuzzy-finder [fzf](https://github.com/junegunn/fzf) written by Junegunn Choi.
# Although the script is very short and simple, it is very convenient and can
# very quickly become an important part of your workflow.
#
# [fzf](https://github.com/junegunn/fzf) is required to use this plugin. Once
# this condition is met, it is sufficient to source the script from your shell
# configuration file. If you are using zsh, the plugin is also compatible with
# the main plugin managers. In the case of zgen, it can be loaded by adding in
# the plugin list.
#
## Usage
#
# Most of the key mappings in the search window are the default fzf ones. The
# most relevant ones are:
# - **ctrl-n** / **ctrl-p** to go to the next/previous match.
# - **ctrl-y** or **Enter** to accept a match.
# - **ctrl-t** to toggle a match for deletion.
#
# In Zsh or Bash, the script creates three commands:
# - **mark** to create a new bookmark. For example, `mark work` creates a
#     bookmark labeled work.
# - **jump** to jump to a given bookmark using fzf. By default, the script binds
#     this function to **ctrl-g**.
# - **dmark** to delete marks toggled for deletion.
#
## Sources
#
# - https://github.com/jocelynmallon/zshmarks
# - https://github.com/junegunn/fzf

if [[ -z $BOOKMARKS_FILE ]] ; then
    export BOOKMARKS_FILE="$HOME/.bookmarks"
fi

if [[ ! -f $BOOKMARKS_FILE ]]; then
    touch $BOOKMARKS_FILE
fi

function mark() {
    echo $@ : $(pwd) >> $BOOKMARKS_FILE
}

fzfcmd() {
   [ ${FZF_TMUX:-1} -eq 1 ] && echo "fzf-tmux -d${FZF_TMUX_HEIGHT:-40%}" || echo "fzf"
}

function jump() {
    local jumpline=$(cat ${BOOKMARKS_FILE} | $(fzfcmd) --bind=ctrl-y:accept --tac)
    if [[ -n ${jumpline} ]]; then
        local jumpdir=$(echo "${jumpline}" | sed -n "s/.* : \(.*\)$/\1/p" | sed "s#~#$HOME#")
        perl -p -i -e "s#${jumpline}\n##g" $BOOKMARKS_FILE
        cd "${jumpdir}" && echo ${jumpline} >> $BOOKMARKS_FILE
    fi
}

function dmark()  {
    local marks_to_delete line
    marks_to_delete=$(cat $BOOKMARKS_FILE | $(fzfcmd) -m --bind=ctrl-y:accept,ctrl-t:toggle-up --tac)

    if [[ -n ${marks_to_delete} ]]; then
        while read -r line; do
            perl -p -i -e "s#${line}\n##g" $BOOKMARKS_FILE
        done <<< "$marks_to_delete"

        echo "** The following marks were deleted **"
        echo "${marks_to_delete}"
    fi
}

bind '"\C-g":"jump\n"'
