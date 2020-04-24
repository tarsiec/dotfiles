#!/bin/sh
sudo pacman -Syy --noconfirm chromium neovim git wget neofetch exa youtube-dl zsh code-oss vim sxiv zathura ranger ttf-joypixels ttf-fira-code xclip xfce4-power-manager dunst zathura-pdf-mupdf alsa-utils thunar materia-gtk-theme
cd ~
mkdir Software
cd Software
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si --noconfirm
yay -S --noconfirm nerd-fonts-iosevka xcolor ttf-material-design-icons-git libxft-bgra
