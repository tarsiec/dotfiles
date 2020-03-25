#               __
#  ____  _____/ /_  __________
# /_  / / ___/ __ \/ ___/ ___/
#  / /_(__  ) / / / /  / /__
# /___/____/_/ /_/_/   \___/	t0maslb@github

### scripts
export PATH=$PATH:"$(du "$HOME/.local/bin" | cut -f2 | tr '\n' ':' | sed 's/:*$//')"

### ENCODING ###
export LANG=en_GB.UTF-8
export LC_ALL=es_ES.UTF-8
### CODE & LANGS ###
export CODE_DIR=$HOME/code
export CODE_RSC=$CODE_DIR/resources
export CODE_PRJ=$CODE_DIR/projects
# rust
export RUSTUP_HOME=$CODE_RSC/rust/rustup
export CARGO_HOME=$CODE_RSC/rust/cargo
export PATH=$PATH:$CARGO_HOME/bin
export PATH=$PATH:
# go
export GOPATH=$CODE_PRJ/go
export PATH=$PATH:$GOPATH/bin
export GOBIN=$GOROOT/bin

export XAUTHORIY="$HOME/.Xauthority"

### ZSH PLUGINS & THEMES ###
export STARSHIP_CONFIG=/home/tomas/.config/starship/starship.toml
source $HOME/.local/share/zsh_plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh

eval "$(starship init zsh)"
eval "$(thefuck --alias)"
eval "$(hub alias -s git)"

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

# KEYBINDINGS
bindkey  "^[[H"   beginning-of-line
bindkey  "^[[F"   end-of-line
bindkey "\e[3~" delete-char

/opt/shell-color-scripts/crunchbang-mini
