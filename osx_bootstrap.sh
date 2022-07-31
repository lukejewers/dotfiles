#!/usr/bin/env bash

# Bootstrap idempotent script for setting up a new OSX machine
echo "starting bootstrapping"

# Check for Homebrew, install if don't have it
if test ! $(which brew); then
    echo "Installing homebrew..."
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# add brew to PATH
echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> /Users/Luke/.zprofile
eval "$(/opt/homebrew/bin/brew shellenv)"

# Update homebrew recipes
brew update

# Install Bash 4
brew install bash

PACKAGES=(
    awk
    deno
    curl
    cmake
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
    rust-analyzer
    tmux
    typescript
    wget
)

echo "Installing packages..."
brew install ${PACKAGES[@]}

echo "Cleaning up..."
brew cleanup

echo "Installing cask..."
CASKS=(
    alfred
    amethyst
    bitwarden
    firefox
    google-chrome
    slack
)
brew install --cask ${CASKS[@]}

echo "Installing fonts..."
FONTS=(
    font-iosevka
)
brew install --cask ${FONTS[@]}

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

echo "Symlinking dotfiles..."
ln -s -f ~/dotfiles/.emacs ~/.emacs
ln -s -f ~/dotfiles/.vimrc ~/.vimrc
ln -s -f ~/dotfiles/.tmux.conf ~/.tmux.conf
ln -s /opt/homebrew/opt/emacs-plus@29/Emacs.app /Applications

echo "Configuring OSX..."
# Enable tap-to-click
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1

# Disable 'Character Picker'
$ defaults write -g ApplePressAndHoldEnabled -bool false

# Set fast key repeat rate
defaults write -g InitialKeyRepeat -int 15
defaults write -g KeyRepeat -int 2

# Show filename extensions by default
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

echo "Bootstrapping complete"
