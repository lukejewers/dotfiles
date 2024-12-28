#!/bin/bash

set -xe

ln -s -f ~/.dotfiles/config.ghostty ~/.config/ghostty/config
ln -s -f ~/.dotfiles/.vimrc ~/.vimrc
ln -s -f ~/.dotfiles/.tmux.conf ~/.tmux.conf
ln -s -f ~/.dotfiles/hammerspoon.lua ~/.hammerspoon/init.lua
mkdir -p ~/.emacs.d && ln -s -f ~/.dotfiles/.emacs ~/.emacs.d/init.el
ln -s -f ~/.dotfiles/DefaultKeybinding.dict ~/Library/KeyBindings/DefaultKeyBinding.Dict
