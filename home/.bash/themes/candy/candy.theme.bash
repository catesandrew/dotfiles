function prompt_command() {
    PS1="${green}\u@\h ${blue}\T ${reset_color}${white}\w${reset_color}$(scm_prompt_info)${blue} →${bold_blue} ${reset_color} ";
}

precmd_functions+=(prompt_command)
