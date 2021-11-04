# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

#!/bin/zsh
#              __
#  ____  _____/ /_  __________
# /_  / / ___/ __ \/ ___/ ___/
#  / /_(__  ) / / / /  / /__
# /___/____/_/ /_/_/   \___/	t0maslb@github

# PROMPT="%F{yellow}%Bλ%b%f (%(?.%B%F{green}%2~%f%b.%B%F{red}%2~%f%b))%(?..%F{red}%B(%?%)%f%b) "
###
# PROMPT="%(?.%B%F{green}%2~%f%b.%B%F{red}%2~ (%?%)%f%b) > "
# PROMPT="[%(?.%B%F{green}%2~%f%b.%B%F{red}%2~%f%b)]%(?..%F{red}%B(%?%)%f%b) "
# PROMPT="%F{blue}%B%n%b%f [%(?.%B%F{green}%2~%f%b.%B%F{red}%2~%f%b)]%(?..%F{red}%B(%?%)%f%b) "
# PROMPT="%F{yellow}%B%n%b%f [%(?.%B%F{green}%2~%f%b.%B%F{red}%2~%f%b)]%(?..%F{red}%B(%?%)%f%b) → "
# PROMPT="%F{yellow}%B%n%b%f [%(?.%B%F{green}%2~%f%b.%B%F{red}%2~%f%b)]%(?..%F{red}%B(%?%)%f%b) → "
# PROMPT="%F{yellow}%B>%b%f %(?.%F{blue}%B%2~ %b%f.%F{red}%B%2~ %f%b)"
# PROMPT="%(?.%F{blue}%B%2~ %b%f.%F{red}%B%2~ %f%b)%F{yellow}%B>%b%f "
# PROMPT="%(?.%F{blue}λ%f %B%F{green}%2~%f%b.%B%F{red}%2~%f%b)%(?..%F{red}%B(%?%)%f%b) "
# PROMPT="%F{yellow}%B%n%b%f@%M [%(?.%B%F{green}%2~%f%b.%B%F{red}%2~%f%b)]%(?..%F{red}%B(%?%)%f%b) "
# PROMPT="%n@%M [%(?.%B%F{green}%1~%f%b].%B%F{red}%1~ (%?%)%f%b) %% "
# PROMPT="%F{yellow}%n%f@%M [%(?.%B%F{green}%1~%f%b].%B%F{red}%1%f%b~ %F{red}%B(%?%)%f%b%) "

# autoload -Uz vcs_info
# precmd_vcs_info() { vcs_info }
# precmd_functions+=( precmd_vcs_info )
# setopt prompt_subst
# RPROMPT="%U\$vcs_info_msg_0_%u"
# zstyle ':vcs_info:git:*' formats '%F{blue}(%b)%r%f'
# zstyle ':vcs_info:*' enable git
###


source "$HOME/.config/zsh/zprofile"
[ -f "/home/tarsiec/.ghcup/env" ] && source "/home/tarsiec/.ghcup/env"

source $HOME/.config/zsh/zprofile
# source $HOME/.config/zsh/ohmyzsh

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
