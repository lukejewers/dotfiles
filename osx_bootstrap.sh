#!/usr/bin/env bash

# Bootstrap idempotent script for setting up a new OSX machine

echo "starting bootstrapping"

# Check for Homebrew, install if don't have it
if test ! $(which brew); then
    echo "Installing homebrew..."
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# Update homebrew recipes
brew update

# Install Bash 4
brew install bash

PACKAGES=(
    awk
    deno
    emacs-plus@28
    gcc
    git
    graphviz
    flake8
    go
    htop
    npm
    node
    postgresql
    python
    python3
    rust
    tmux
    typescript
    wget
)

echo "Installing packages..."
brew install ${PACKAGES[@]}

echo "Cleaning up..."
brew cleanup

echo "Installing cask..."
brew install homebrew/cask

CASKS=(
    google-chrome
    slack
)

echo "Installing fonts..."
brew tap caskroom/fonts
FONTS=(
    font-iosevka
)
brew cask install ${FONTS[@]}

echo "Installing global npm packages..."
npm install -g typescript-language-server typescript

echo "Installing nvm..."
wget -qO- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash

echo "Installing Python packages..."
PYTHON_PACKAGES=(
    ipython
    pyright
    virtualenv
    virtualenvwrapper
)
sudo pip install ${PYTHON_PACKAGES[@]}

# Set terminal font family and size
tell application "Terminal"
    set ProfilesNames to name of every settings set
    repeat with ProfileName in ProfilesNames
        set font name of settings set ProfileName to "Iosevka"
        set font size of settings set ProfileName to 20
    end repeat
end tell

echo "Symlinking dotfiles..."
ln -s -f ~/dotfiles/.emacs ~/.emacs
ln -s -f ~/dotfiles/.vimrc ~/.vimrc
ln -s -f ~/dotfiles/.tmux.conf ~/.tmux.conf

echo "Configuring OSX..."

# Set fast key repeat rate
defaults write NSGlobalDomain KeyRepeat -int 0

# Show filename extensions by default
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

# Enable tap-to-click
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1

echo "Bootstrapping complete"
