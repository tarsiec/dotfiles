#              __
#  ____  _____/ /_  __________
# /_  / / ___/ __ \/ ___/ ___/
#  / /_(__  ) / / / /  / /__
# /___/____/_/ /_/_/   \___/	t0maslb@github

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.

# ohmyzsh
export ZSH="/home/tomas/.oh-my-zsh"
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

# ZSH_THEME="robbyrussell"
# ZSH_THEME="agnoster"
ZSH_THEME="powerlevel10k/powerlevel10k"
# ZSH_THEME="spaceship"
# ZSH_THEME="lambda-gitster"
# ZSH_THEME="typewritten"
# ZSH_THEME="elessar"


plugins=(
	git
	colored-man-pages
	zsh-syntax-highlighting
)
source $ZSH/oh-my-zsh.sh

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

eval "$(thefuck --alias)"

alias getvid="youtube-dl --restrict-filenames -f 22"
alias getaudio="youtube-dl --restrict-filenames -x --audio-format mp3"
alias dmi="doas make install"
alias dmci="doas make clean install"
alias drc="sudo make install & pkill dwm"
alias rcst="rm config.h & make & sudo make install"
