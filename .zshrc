#              __
#  ____  _____/ /_  __________
# /_  / / ___/ __ \/ ___/ ___/
#  / /_(__  ) / / / /  / /__
# /___/____/_/ /_/_/   \___/	t0maslb@github

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.

# scripts
export PATH=$PATH:"$(printf "%s:" ${HOME}/.local/bin/*)"
# encoding
export LANG=en_GB.UTF-8
export LC_ALL=es_ES.UTF-8
# code & langs
export CODE_DIR=$HOME/code
export CODE_RSC=$CODE_DIR/resources
export CODE_PRJ=$CODE_DIR/projects
# rust
export RUSTUP_HOME=$HOME/code/resources/rustup
export CARGO_HOME=$HOME/code/resources/cargo
export PATH=$PATH:$CARGO_HOME/bin
# go
export GOPATH=$CODE_PRJ/go
export PATH=$PATH:$GOPATH/bin
#export GOROOT=/usr/lib/go
export GOBIN=$GOROOT/bin

source $HOME/.local/bin/zsh_plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

eval "$(starship init zsh)"
eval "$(thefuck --alias)"

alias getvid="youtube-dl --restrict-filenames -f 22"
alias getaudio="youtube-dl --restrict-filenames -x --audio-format mp3"
alias dmi="doas make install"
alias dmci="doas make clean install"
alias drc="sudo make install & pkill dwm"
alias rcst="rm config.h & make & sudo make install"
alias ls="ls --color=auto"
alias l="ls -la"
alias v="vim"

# KEYBINDINGS
bindkey  "^[[H"   beginning-of-line
bindkey  "^[[F"   end-of-line
bindkey "\e[3~" delete-char

/opt/shell-color-scripts/crunchbang-mini
