#!/bin/bash

# Run as root, or use sudo

ln -s ~/dotfiles/i3 ~/.config/
ln -s ~/dotfiles/i3status ~/.config/
ln -s ~/dotfiles/nvim ~/.config/
ln -s ~/dotfiles/picom ~/.config/
ln -s ~/dotfiles/rofi ~/.config/
ln -s ~/dotfiles/sway ~/.config/
ln -s ~/dotfiles/polybar ~/.config/
ln -s ~/dotfiles/kitty ~/.config/
ln -s ~/dotfiles/fonts/PxPlus_IBM_EGA_8x14.ttf /usr/share/fonts/
ln -s ~/dotfiles/dunst ~/.config/

rm ~/.bashrc
ln -s ~/dotfiles/.bashrc ~/
rm ~/.bash_profile
ln -s ~/dotfiles/.bash_profile ~/
rm ~/.tmux.conf
ln -s ~/dotfiles/.tmux.conf ~/
rm ~/.zshrc
ln -s ~/dotfiles/.zshrc ~/
