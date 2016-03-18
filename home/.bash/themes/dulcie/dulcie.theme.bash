# Simplistic one-liner theme to display source control management info beside
# the ordinary bash prompt.

prompt() {
  PS1="${reset_color}[\u@\h$(scm_prompt_info)${reset_color} \W]\$ "
}

precmd_functions+=(prompt)
