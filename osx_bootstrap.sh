#!/usr/bin/env bash

echo "starting bootstrapping"

# Check for Homebrew, install if don't have it
if test ! $(which brew); then
    echo "Installing homebrew..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

# add brew to PATH
echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> /Users/Luke/.zprofile
eval "$(/opt/homebrew/bin/brew shellenv)"

# Update homebrew recipes
brew update

# Install Bash 4
brew install bash

PACKAGES=(
    curl
    deno
    gcc
    git
    go
    node
    npm
    postgresql
    tmux
    typescript
    typescript-language-server
    wget
)

echo "Installing packages..."
brew install ${PACKAGES[@]}

echo "Cleaning up..."
brew cleanup

echo "Installing cask..."
CASKS=(
    doll
    google-chrome
    hammerspoon
    kitty
    raycast
)
brew install --cask ${CASKS[@]}

echo "Installing fonts..."
FONTS=(
    font-iosevka
)
brew install --cask ${FONTS[@]}

echo "Installing nvm..."
wget -qO- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash

echo "Updating pip..."
python3 -m pip install --upgrade pip

echo "Installing Python packages..."
PYTHON_PACKAGES=(
    ipython
    pyright
)
sudo pip install ${PYTHON_PACKAGES[@]}

echo "Installing emacs..."
brew tap d12frosted/emacs-plus
brew install emacs-plus@29 --with-native-comp

echo "Symlinking dotfiles..."
mkdir -p ~/.emacs.d
ln -s -f ~/dotfiles/.vimrc ~/.vimrc
ln -s -f ~/dotfiles/.tmux.conf ~/.tmux.conf
ln -s -f ~/dotfiles/hammerspoon.lua ~/.hammerspoon/init.lua
ln -s -f ~/dotfiles/.emacs ~/.emacs.d/init.el
ln -s -f ~/dotfiles/DefaultKeybinding.dict ~/Library/KeyBindings/DefaultKeyBinding.Dict
ln -s /opt/homebrew/opt/emacs-plus@29/Emacs.app /Applications

echo "Configuring OSX..."
# Set fast key repeat rate
defaults write -g InitialKeyRepeat -int 15
defaults write -g KeyRepeat -int 2

# Show filename extensions by default
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

# Update terminal prompt
echo 'PS1="%n%f %~ $ "' >> ~/.zshrc

echo "Bootstrapping complete"
