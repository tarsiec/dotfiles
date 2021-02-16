#              __
#  ____  _____/ /_  __________
# /_  / / ___/ __ \/ ___/ ___/
#  / /_(__  ) / / / /  / /__
# /___/____/_/ /_/_/   \___/	t0maslb@github

###
# PROMPT="%(?.%B%F{green}%1~%f%b.%B%F{red}%1~ (%?%)%f%b) λ "
PROMPT="%B%F{yellow}%n%f%b@%M [%(?.%B%F{green}%2~%f%b.%B%F{red}%2~%f%b)]%(?..%F{red}%B(%?%)%f%b) "
# PROMPT="%n@%M [%(?.%B%F{green}%1~%f%b].%B%F{red}%1~ (%?%)%f%b) %% "
# PROMPT="%F{yellow}%n%f@%M [%(?.%B%F{green}%1~%f%b].%B%F{red}%1%f%b~ %F{red}%B(%?%)%f%b%) "

autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
RPROMPT="%U\$vcs_info_msg_0_%u"
zstyle ':vcs_info:git:*' formats '%F{blue}(%b)%r%f'
zstyle ':vcs_info:*' enable git
###

source $HOME/.config/zsh/zprofile
