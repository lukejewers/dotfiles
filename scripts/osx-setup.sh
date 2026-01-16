#!/usr/bin/env bash

set -xe

echo "Cloning dotfiles..."
if [ ! -d "$HOME/.dotfiles" ]; then
    git clone https://github.com/lukejewers/dotfiles.git "$HOME/.dotfiles"
else
    echo "Dotfiles already exist, pulling latest changes..."
    cd "$HOME/.dotfiles" && git pull
fi

mkdir -p "$HOME/.emacs.d"
mkdir -p "$HOME/.config/ghostty"
mkdir -p "$HOME/.hammerspoon"
mkdir -p "$HOME/Library/KeyBindings"
mkdir -p "$HOME/Screenshots"

echo "Installing system deps..."
command -v brew >/dev/null 2>&1 || { echo "Homebrew not found. Install it first."; exit 1; }
brew bundle --file="$HOME/.dotfiles/.brewfile"

echo "Configuring OSX..."
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults write -g InitialKeyRepeat -int 15
defaults write -g KeyRepeat -int 2
defaults write NSGlobalDomain AppleShowAllExtensions -bool true
defaults write NSGlobalDomain NSAutomaticSpellCheckingEnabled -bool false
defaults write com.apple.LaunchServices LSQuarantine -bool false
defaults write com.apple.controlcenter.plist BatteryShowPercentage -bool true
defaults write com.apple.dock autohide -bool true
defaults write com.apple.dock autohide-delay -float 0
defaults write com.apple.dock autohide-time-modifier -float 0.5
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults write com.apple.finder AppleShowAllFiles -bool true
defaults write com.apple.screencapture location -string "$HOME/Screenshots"
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 70 '<dict><key>enabled</key><false/></dict>'

for app in "Finder" "Dock" "SystemUIServer"; do
    killall "${app}" &> /dev/null || true
done

echo "Setting symlinks..."
ln -sf "$HOME/.dotfiles/.defaultkeybinding" "$HOME/Library/KeyBindings/DefaultKeyBinding.dict"
ln -sf "$HOME/.dotfiles/.emacs" "$HOME/.emacs.d/init.el"
ln -sf "$HOME/.dotfiles/.ghostty" "$HOME/.config/ghostty/config"
ln -sf "$HOME/.dotfiles/.hammerspoon" "$HOME/.hammerspoon/init.lua"
ln -sf "$HOME/.dotfiles/.keyremapping" "$HOME/Library/LaunchAgents/com.local.KeyRemapping.plist"
ln -sf "$HOME/.dotfiles/.vimrc" "$HOME/.vimrc"

[ -e /opt/homebrew/opt/emacs-plus@31/Emacs.app ] && ln -sf /opt/homebrew/opt/emacs-plus@31/Emacs.app /Applications

echo "Creating .zshrc loader..."
cat > "$HOME/.zshrc" << 'EOF'
#!/bin/zsh
#
# Machine-specific zsh configuration loader
# This file is NOT version controlled

# Load shared base configuration
[ -f $HOME/.dotfiles/.zshrc.base ] && source $HOME/.dotfiles/.zshrc.base

# --- Machine-Specific Settings Below This Line ---
EOF

echo "Configuration complete."
