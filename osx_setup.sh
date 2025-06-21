#!/usr/bin/env bash

# Update terminal prompt
echo 'PS1="%n@%m%f %~ $ "' >> ~/.zshrc

echo "Configuring OSX..."
# Set fast key repeat rate
defaults write -g InitialKeyRepeat -int 15
defaults write -g KeyRepeat -int 2
# Show filename extensions by default
defaults write NSGlobalDomain AppleShowAllExtensions -bool true
# Disable the "Are you sure you want to open this application?" dialog
defaults write com.apple.LaunchServices LSQuarantine -bool false
# Disable auto-correct
defaults write NSGlobalDomain NSAutomaticSpellCheckingEnabled -bool false
# Enable tap to click (Trackpad)
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
# Allow C-M-d
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 70 '<dict><key>enabled</key><false/></dict>'
# Show hidden files in Finder
defaults write com.apple.finder AppleShowAllFiles -bool true
# Save screenshots to a specific location (e.g., Desktop)
mkdir -p "${HOME}/Screenshots"
defaults write com.apple.screencapture location -string "${HOME}/Screenshots"
# Set dock & menubar to auto-hide
defaults write com.apple.dock autohide -bool true
defaults write NSGlobalDomain _HIHideMenuBar -bool true
# Make dock appear faster when auto-hidden
defaults write com.apple.dock autohide-delay -float 0
defaults write com.apple.dock autohide-time-modifier -float 0.5
# Kill affected applications to apply changes
for app in "Finder" "Dock" "SystemUIServer"; do
    killall "${app}" &> /dev/null
done

echo "Cloning dotfiles..."
if [ ! -d "$HOME/.dotfiles" ]; then
  git clone https://github.com/lukejewers/dotfiles.git "$HOME/.dotfiles"
fi

echo "Setting symlinks..."
mkdir -p ~/.emacs.d
mkdir -p ~/.config/ghostty
mkdir -p ~/.hammerspoon

ln -s -f ~/.dotfiles/.vimrc ~/.vimrc
ln -s -f ~/.dotfiles/DefaultKeybinding.dict ~/Library/KeyBindings/DefaultKeyBinding.Dict
ln -s -f ~/.dotfiles/config.ghostty ~/.config/ghostty/config
ln -s -f ~/.dotfiles/hammerspoon.lua ~/.hammerspoon/init.lua
ln -s -f ~/.dotfiles/init.el ~/.emacs.d/init.el

ln -s /opt/homebrew/opt/emacs-plus@30/Emacs.app /Applications

echo "Configuration complete."
