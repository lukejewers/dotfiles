#!/bin/bash

sudo xbps-install -Syu
sudo xbps-install -y void-repo-nonfree
sudo xbps-install -Syu

sudo xbps-install -y \
    linux-firmware-amd \
    mesa-dri mesa-vaapi mesa-vdpau vulkan-radeon libgbm \
    tlp elogind dbus

sudo xbps-install -y \
    sway waybar ghostty dmenu rofi \
    wl-clipboard nautilus wget xtools git \
    firefox emacs-pgtk \
    font-iosevka nerd-fonts font-iosevka-nerd

sudo xbps-install -y \
    pipewire wireplumber pipewire-pulse alsa-pipewire \
    pavucontrol sof-firmware

echo "Setting up dotfiles..."
rm -rf ~/.dotfiles
git clone https://github.com/lukejewers/dotfiles.git ~/.dotfiles

mkdir -p ~/.config/sway
mkdir -p ~/.config/waybar
mkdir -p ~/.config/ghostty

ln -sf ~/.dotfiles/.sway ~/.config/sway/config
ln -sf ~/.dotfiles/.waybar.config ~/.config/waybar/config.jsonc
ln -sf ~/.dotfiles/.waybar.style ~/.config/waybar/style.css
ln -sf ~/.dotfiles/.ghostty ~/.config/ghostty/config

sudo ln -sf /etc/sv/dbus /var/service/
sudo ln -sf /etc/sv/elogind /var/service/
sudo ln -sf /etc/sv/tlp /var/service/

sudo gpasswd -a $(whoami) _seatd
sudo gpasswd -a $(whoami) video
sudo gpasswd -a $(whoami) audio

mkdir -p ~/.config/pipewire/pipewire.conf.d
ln -sf /usr/share/examples/wireplumber/10-wireplumber.conf ~/.config/pipewire/pipewire.conf.d/
ln -sf /usr/share/examples/pipewire/20-pipewire-pulse.conf ~/.config/pipewire/pipewire.conf.d/

if ! grep -q "MOZ_ENABLE_WAYLAND" ~/.bashrc; then
    echo 'export XDG_RUNTIME_DIR=/run/user/$(id -u)' >> ~/.bashrc
    echo 'export MOZ_ENABLE_WAYLAND=1' >> ~/.bashrc
    echo 'export XDG_CURRENT_DESKTOP=sway' >> ~/.bashrc
fi

echo "Done! Please reboot."
