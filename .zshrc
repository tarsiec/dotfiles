# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

#               __
#  ____  _____/ /_  __________
# /_  / / ___/ __ \/ ___/ ___/
#  / /_(__  ) / / / /  / /__
# /___/____/_/ /_/_/   \___/	t0maslb@github


### ohmyzsh
export ZSH="$HOME/.oh-my-zsh"

### DEFAULT SOFTWARE ###
export EDITOR=nvim
export TERM=st
export VISUAL=nvim

### scripts
export PATH=$PATH:"$(du "$HOME/.local/bin" | cut -f2 | tr '\n' ':' | sed 's/:*$//')"
### ENCODING ###
# export LANG="es_ES.UTF-8"
# export LC_ALL="es_ES.UTF-8"
# export LANGUAGE="es"
# export LC_CTYPE="es_ES.UTF-8"
# export LC_NUMERIC="es_ES.UTF-8"
# export LC_TIME="es_ES.UTF-8"
# export LC_COLLATE="es_ES.UTF-8"
# export LC_MONETARY="es_ES.UTF-8"
# export LC_MESSAGES="es_ES.UTF-8"
# export LC_PAPER="es_ES.UTF-8"
# export LC_NAME="es_ES.UTF-8"
# export LC_ADDRESS="es_ES.UTF-8"
# export LC_TELEPHONE="es_ES.UTF-8"
# export LC_MEASUREMENT="es_ES.UTF-8"
# export LC_IDENTIFICATION="es_ES.UTF-8"
# export LC_ALL="es_ES.UTF-8"
### CODE & LANGS ###
export CODE_DIR=$HOME/Code
export CODE_RSC=$CODE_DIR/resources
export CODE_PRJ=$CODE_DIR/projects
# rust
export RUSTUP_HOME=$CODE_RSC/rust/rustup
export CARGO_HOME=$CODE_RSC/rust/cargo
export PATH=$PATH:$CARGO_HOME/bin
# go
export GOPATH=$CODE_PRJ/go
export GOBIN=$GOPATH/bin
export PATH=$PATH:$GOBIN
### DO NOT CHANGE ###
export XAUTHORIY="$HOME/.Xauthority"
### starship.rs
export STARSHIP_CONFIG=$HOME/.config/starship/starship.toml

source $ZSH/oh-my-zsh.sh
source $HOME/.local/share/zsh_plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $HOME/.local/share/zsh_plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
fpath=(.local/share/zsh_plugins/zsh-completions/zsh-completions/src $fpath)
# source /home/tomas/.local/share/zsh_plugins/zsh-autosuggestions/zsh-autosuggestions.zsh


### ZSH PLUGINS & THEMES ###
# ZSH_THEME="agnoster"
# plugins=(git)

### THEME ###
eval "$(starship init zsh)"


eval "$(thefuck --alias)"

alias getvid="youtube-dl --restrict-filenames -f 22"
alias getaudio="youtube-dl --restrict-filenames -x --audio-format mp3"
alias dmi="doas make install"
alias dmci="doas make clean install"
alias drc="sudo make install & pkill dwm"
alias rcst="rm config.h & make & sudo make install"
alias ls="exa"
alias l="exa -lag"
alias lsize="exa -lags size"
alias lname="exa -lags name"
alias v="vim"
alias nv="nvim"
alias -g Z="| fzf"
alias mv="mv -i"
alias m="mocp"
alias screenshot="scrot"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
