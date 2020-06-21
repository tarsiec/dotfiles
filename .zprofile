### ohmyzsh
#export ZSH="$HOME/.oh-my-zsh"

### DEFAULT SOFTWARE ###
export TERM="alacritty"
export EDITOR="nvim"
export VISUAL="nvim"
export READER="zathura"
export BROWSER="brave"
export PAGER="less"

### scripts
export PATH="$PATH:$(du "$HOME/.local/bin/" | cut -f2 | paste -sd ':')"

export ZSH="/home/tlb/.oh-my-zsh"
### CODE & LANGS ###
export CODE_DIR=$HOME/code
export CODE_RSC=$CODE_DIR/resources
export CODE_PRJ=$CODE_DIR/projects
# rust
export RUSTUP_HOME=$CODE_RSC/rust/rustup
#export RUST_SRC=$CODE_PRJ/rust
export CARGO_HOME=$CODE_RSC/rust/cargo
export PATH=$PATH:$CARGO_HOME/bin
export PATH="/home/tlb/code/resources/rust/cargo/bin:$PATH"
# go
export GOPATH=$CODE_PRJ/go
export GOBIN=$GOPATH/bin
export PATH=$PATH:$GOBIN

### SOFTWARE ###
# starship.rs
export STARSHIP_CONFIG=$HOME/.config/starship/starship.toml

### "PLUGINS" ###
source $HOME/.local/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $HOME/.local/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
#source $HOME/.local/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# oh my zsh
source $ZSH/oh-my-zsh.sh

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
alias c="calcurse"

alias gc="cd ~/.config"
alias gcb="cd ~/.config/bspwm"
alias gcn="cd ~/.config/nvim"
alias gca="cd ~/.config/alacritty"
alias gcp="cd ~/.config/polybar"
alias gcs="cd ~/.config/sxhkd"
alias gcl="cd ~/.config/lf"
alias gcd="cd ~/.config/dunst"
alias gcz="cd ~/.config/zathura"

alias gtf="cd ~/.local/trash/files"

alias gv="cd ~/vids"
alias gvy="cd ~/vids/yt"

alias gp="cd ~/pics"
alias gpw="cd ~/pics/wallpapers"
alias gps="cd ~/pics/screenshots"

alias gd="cd ~/docs"
alias gdi="cd ~/docs/instituto"
alias gdr="cd ~/docs/reads"

alias gb="cd ~/.local/bin/"
alias gbd="cd ~/.local/bin/dwmbar"
alias gbp="cd ~/.local/bin/polybar"
alias gbb="cd ~/.local/bin/bspwm"
alias gbt="cd ~/.local/bin/transmission"

alias gr="cd ~/.local/share/repos"
alias grd="cd ~/.local/share/repos/dotfiles"
alias gpm="cd ~/.local/share/pkg/dmenu"
alias gpw="cd ~/.local/share/pkg/dwm"
alias gpt="cd ~/.local/share/pkg/tabbed"
alias gps="cd ~/.local/share/pkg/st"

alias gc="cd ~/.config"
alias gE="cd /etc"
alias gU="cd /usr"
alias gUs="cd /usr/share"

export LF_ICONS="di=📁:\
fi=📃:\
tw=🤝:\
ow=📂:\
ln=⛓:\
or=❌:\
ex=🎯:\
*.txt=✍:\
*.mom=✍:\
*.me=✍:\
*.ms=✍:\
*.png=🖼:\
*.ico=🖼:\
*.jpg=📸:\
*.jpeg=📸:\
*.gif=🖼:\
*.svg=🗺:\
*.xcf=🖌:\
*.html=🌎:\
*.xml=📰:\
*.gpg=🔒:\
*.css=🎨:\
*.pdf=📚:\
*.djvu=📚:\
*.epub=📚:\
*.csv=📓:\
*.xlsx=📓:\
*.tex=📜:\
*.md=📘:\
*.r=📊:\
*.R=📊:\
*.rmd=📊:\
*.Rmd=📊:\
*.mp3=🎵:\
*.opus=🎵:\
*.ogg=🎵:\
*.m4a=🎵:\
*.flac=🎼:\
*.mkv=🎥:\
*.mp4=🎥:\
*.webm=🎥:\
*.mpeg=🎥:\
*.avi=🎥:\
*.zip=📦:\
*.rar=📦:\
*.7z=📦:\
*.tar.gz=📦:\
*.z64=🎮:\
*.v64=🎮:\
*.n64=🎮:\
*.1=ℹ:\
*.nfo=ℹ:\
*.info=ℹ:\
*.log=📙:\
*.iso=📀:\
*.img=📀:\
*.bib=🎓:\
*.ged=👪:\
*.part=💔:\
*.torrent=🔽:\
"
