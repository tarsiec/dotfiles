### ohmyzsh
#export ZSH="$HOME/.oh-my-zsh"

### DEFAULT SOFTWARE ###
export EDITOR="nvim"
export TERM="alacritty"
export VISUAL="nvim"
export READER="zathura"
export BROWSER="brave"

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

### ZSH PLUGINS ###
source $HOME/.local/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $HOME/.local/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
fpath=(.local/share/zsh/plugins/zsh-completions/zsh-completions/src $fpath)
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
