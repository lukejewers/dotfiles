#!/bin/bash

echo "Updating system and adding non-free repo..."
sudo xbps-install -Syu
sudo xbps-install -y void-repo-nonfree
sudo xbps-install -Syu

echo "Installing Drivers and Core Services..."
sudo xbps-install -y \
    linux-firmware-amd \
    mesa-dri mesa-vaapi mesa-vdpau vulkan-radeon libgbm \
    tlp elogind dbus bluez

echo "Installing Audio Stack (Pipewire)..."
sudo xbps-install -y \
    pipewire wireplumber pipewire-pulse alsa-pipewire \
    pavucontrol sof-firmware alsa-utils

echo "Installing Desktop Environment and Apps..."
sudo xbps-install -y \
    gsettings-desktop-schemas sway waybar ghostty rofi \
    nautilus firefox emacs-pgtk \
    cmake libtool gdb git wget xtools wl-clipboard \
    font-iosevka-nerd nerd-fonts font-logos jq ffmpeg yt-dlp

echo "Setting up dotfiles..."
DOTFILES="$HOME/.dotfiles"
if [ -d "$DOTFILES" ]; then
    echo "Dotfiles dir exists, pulling latest..."
    cd "$DOTFILES" && git pull
else
    git clone https://github.com/lukejewers/dotfiles.git "$DOTFILES"
fi

mkdir -p ~/.config/sway ~/.config/waybar ~/.config/ghostty ~/.emacs.d
ln -sf "$DOTFILES"/.sway ~/.config/sway/config
ln -sf "$DOTFILES"/.waybar.config ~/.config/waybar/config.jsonc
ln -sf "$DOTFILES"/.waybar.style ~/.config/waybar/style.css
ln -sf "$DOTFILES"/.ghostty ~/.config/ghostty/config
ln -sf "$DOTFILES"/.emacs ~/.emacs.d/init.el

echo "Enabling services..."
sudo ln -sf /etc/sv/dbus /var/service/
sudo ln -sf /etc/sv/elogind /var/service/
sudo ln -sf /etc/sv/tlp /var/service/
sudo ln -sf /etc/sv/bluetoothd /var/service/

echo "Adding user to groups..."
for group in video audio bluetooth; do
    sudo gpasswd -a "$USER" $group
done

echo "Configuring Pipewire ALSA..."
sudo mkdir -p /etc/alsa/conf.d
sudo ln -sf /usr/share/alsa/alsa.conf.d/50-pipewire.conf /etc/alsa/conf.d/
sudo ln -sf /usr/share/alsa/alsa.conf.d/99-pipewire-default.conf /etc/alsa/conf.d/

echo "Setting environment variables..."
if ! grep -q "MOZ_ENABLE_WAYLAND" $HOME/.bashrc; then
    {
      echo 'export MOZ_ENABLE_WAYLAND=1'
      echo 'export XDG_CURRENT_DESKTOP=sway'
    } >> $HOME/.bashrc
fi

echo "Done! Please reboot."
