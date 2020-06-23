#!/bin/sh
cd ~
mkdir -p docs pics vids code music .local/share/pkg
sudo pacman -Syy --noconfirm brave neovim git wget neofetch exa youtube-dl zsh sxiv zathura ranger ttf-joypixels ttf-fira-code xclip xfce4-power-manager dunst zathura-pdf-mupdf alsa-utils thunar materia-gtk-theme
cd ~/.local/share/pkg
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si --noconfirm
yay -S --noconfirm nerd-fonts-iosevka xcolor ttf-material-design-icons-git libxft-bgra ttf-jetbrains-mono
echo "[DONE], now copy or link the dotfiles to their adresses" #TODO automatically put them where they need 2 b@
