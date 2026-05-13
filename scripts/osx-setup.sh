#!/usr/bin/env bash

set -xe

DOTFILES="$HOME/dotfiles"

echo "Cloning dotfiles..."
if [ ! -d "$DOTFILES" ]; then
    git clone https://github.com/lukejewers/dotfiles.git "$DOTFILES"
else
    echo "Dotfiles already exist, pulling latest changes..."
    cd "$DOTFILES" && git pull
fi

mkdir -p "$HOME/.emacs.d"
mkdir -p "$HOME/Library/KeyBindings"
mkdir -p "$HOME/Screenshots"

echo "Installing system deps..."
command -v brew >/dev/null 2>&1 || { echo "Homebrew not found. Install it first."; exit 1; }
brew bundle --file="$DOTFILES/.brewfile"

echo "Configuring OSX..."
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults write -g InitialKeyRepeat -int 14
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
ln -sf "$DOTFILES/defaultkeybinding" "$HOME/Library/KeyBindings/DefaultKeyBinding.dict"
ln -sf "$DOTFILES/init.el" "$HOME/.emacs.d/init.el"
ln -sf "$DOTFILES/keyremapping" "$HOME/Library/LaunchAgents/com.local.KeyRemapping.plist"
ln -sf "$DOTFILES/vimrc" "$HOME/.vimrc"

[ -e /opt/homebrew/opt/emacs-plus@31/Emacs.app ] && ln -sf /opt/homebrew/opt/emacs-plus@31/Emacs.app /Applications

echo "Creating .zshrc loader..."
cat > "$HOME/.zshrc" << EOF
#!/bin/zsh
#
# Machine-specific zsh configuration loader
# This file is NOT version controlled

# Load shared base configuration
[ -f $DOTFILES/zshrc ] && source $DOTFILES/zshrc

# --- Machine-Specific Settings Below This Line ---
EOF

echo "Configuration complete."
