#!/bin/bash

set -xe

mkdir -p ~/.config/ghostty
mkdir -p ~/.hammerspoon
mkdir -p ~/.emacs.d

ln -s -f ~/.dotfiles/config.ghostty ~/.config/ghostty/config
ln -s -f ~/.dotfiles/.vimrc ~/.vimrc
ln -s -f ~/.dotfiles/.tmux.conf ~/.tmux.conf
ln -s -f ~/.dotfiles/hammerspoon.lua ~/.hammerspoon/init.lua
ln -s -f ~/.dotfiles/DefaultKeybinding.dict ~/Library/KeyBindings/DefaultKeyBinding.Dict

ln -s -f ~/.dotfiles/init.el ~/.emacs.d/init.el
ln -s -f ~/.dotfiles/early-init.el ~/.emacs.d/early-init.el

ln -s /opt/homebrew/opt/emacs-plus@30/Emacs.app /Applications
