#!/bin/bash

set -euo pipefail

echo "Authenticating sudo..."
sudo -v

while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

echo "Updating system and adding non-free repo..."
sudo xbps-install -Syu
sudo xbps-install -y void-repo-nonfree
sudo xbps-install -Syu

echo "Installing Drivers and Core Services..."
sudo xbps-install -y \
    mesa-dri linux-firmware tlp elogind dbus iwd \
    pipewire wireplumber alsa-pipewire libspa-bluetooth bluez \
    pavucontrol alsa-utils

echo "Installing Desktop Environment and Apps..."
sudo xbps-install -y \
    sway i3status-rust rofi \
    xdg-desktop-portal-wlr xdg-desktop-portal-gtk slurp \
    nautilus emacs-pgtk foot zsh firefox \
    cmake libtool gdb git wget ripgrep xtools wl-clipboard \
    nerd-fonts font-iosevka jq ffmpeg yt-dlp \
    man-pages-devel man-pages-posix mpv

echo "Rebuilding font cache..."
fc-cache -fv

echo "Setting up dotfiles..."
DOTFILES="$HOME/dotfiles"
if [ -d "$DOTFILES" ]; then
    echo "Dotfiles dir exists, pulling latest..."
    git -C "$DOTFILES" pull
else
    git clone https://github.com/lukejewers/dotfiles.git "$DOTFILES"
fi

echo "Symlinking user configs..."
mkdir -p "$HOME/.config/sway" "$HOME/.config/i3status-rust" \
ln -sf "$DOTFILES/.sway" "$HOME/.config/sway/config"
ln -sf "$DOTFILES/.i3status-rust" "$HOME/.config/i3status-rust/config.toml"
ln -sf "$DOTFILES/.emacs" "$HOME/.emacs.d/init.el"

echo "Enabling key remap..."
sudo mkdir -p /etc/udev/hwdb.d
sudo cp "$DOTFILES/.hwdb" /etc/udev/hwdb.d/90-keyboard-custom.hwdb
sudo udevadm hwdb --update
sudo udevadm trigger

echo "Enabling services..."
sudo ln -sf /etc/sv/dbus /var/service/
sudo ln -sf /etc/sv/elogind /var/service/
sudo ln -sf /etc/sv/tlp /var/service/
sudo ln -sf /etc/sv/bluetoothd /var/service/
sudo ln -sf /etc/sv/iwd /var/service/

echo "Adding user to groups..."
for group in video audio bluetooth; do
    sudo gpasswd -a "$USER" "$group"
done

echo "Configuring Pipewire ALSA..."
sudo mkdir -p /etc/alsa/conf.d
sudo ln -sf /usr/share/alsa/alsa.conf.d/50-pipewire.conf /etc/alsa/conf.d/
sudo ln -sf /usr/share/alsa/alsa.conf.d/99-pipewire-default.conf /etc/alsa/conf.d/

echo "Setting environment variables..."
touch "$HOME/.zprofile"
if ! grep -q "MOZ_ENABLE_WAYLAND" "$HOME/.zprofile"; then
    {
      echo 'export MOZ_ENABLE_WAYLAND=1'
      echo 'export XDG_CURRENT_DESKTOP=sway'
    } >> "$HOME/.zprofile"
fi

echo "Creating .zshrc loader..."
cat > "$HOME/.zshrc" << EOF
#!/bin/zsh
#
# Machine-specific zsh configuration loader
# This file is NOT version controlled

# Load shared base configuration
[ -f $DOTFILES/.zshrc.base ] && source $DOTFILES/.zshrc.base

# --- Machine-Specific Settings Below This Line ---
EOF

echo "Done! Please reboot."
