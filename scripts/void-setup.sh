#!/bin/bash

sudo xbps-install -Syu
sudo xbps-install -y void-repo-nonfree
sudo xbps-install -Syu

sudo xbps-install -y \
    linux-firmware-amd \
    mesa-dri mesa-vaapi mesa-vdpau vulkan-radeon libgbm \
    tlp elogind dbus bluez

sudo xbps-install -y \
    gsettings-desktop-schemas sway waybar ghostty rofi \
    nautilus firefox emacs-pgtk \
    cmake libtool gdb git wget xtools wl-clipboard \
    font-iosevka-nerd nerd-fonts font-logos


sudo xbps-install -y \
    pipewire wireplumber pipewire-pulse alsa-pipewire \
    pavucontrol sof-firmware alsa-utils

echo "Setting up dotfiles..."
rm -rf ~/.dotfiles
git clone https://github.com/lukejewers/dotfiles.git ~/.dotfiles

mkdir -p ~/.config/sway ~/.config/waybar ~/.config/ghostty ~/.emacs.d

ln -sf ~/.dotfiles/.sway ~/.config/sway/config
ln -sf ~/.dotfiles/.waybar.config ~/.config/waybar/config.jsonc
ln -sf ~/.dotfiles/.waybar.style ~/.config/waybar/style.css
ln -sf ~/.dotfiles/.ghostty ~/.config/ghostty/config
ln -sf ~/.dotfiles/.emacs ~/.emacs.d/init.el

sudo ln -sf /etc/sv/dbus /var/service/
sudo ln -sf /etc/sv/elogind /var/service/
sudo ln -sf /etc/sv/tlp /var/service/
sudo ln -sf /etc/sv/bluetoothd /var/service/

sudo gpasswd -a $USER video
sudo gpasswd -a $USER audio
sudo gpasswd -a $USER bluetooth

sudo mkdir -p /etc/alsa/conf.d
sudo ln -sf /usr/share/alsa/alsa.conf.d/50-pipewire.conf /etc/alsa/conf.d/
sudo ln -sf /usr/share/alsa/alsa.conf.d/99-pipewire-default.conf /etc/alsa/conf.d/

echo "Applying GSettings..."
apply_gs() {
    if [ -z "$WAYLAND_DISPLAY" ] && [ -z "$DISPLAY" ]; then
        # in a TTY, need a temporary DBus session
        dbus-run-session gsettings set "$1" "$2" "$3"
    else
        # in Sway
        gsettings set "$1" "$2" "$3"
    fi
}

apply_gs org.gnome.desktop.interface font-name 'Iosevka 16'
apply_gs org.gnome.desktop.interface monospace-font-name 'Iosevka Nerd Font 16'
apply_gs org.gnome.desktop.interface document-font-name 'Iosevka 16'
apply_gs org.gnome.desktop.wm.preferences titlebar-font 'Iosevka Bold 16'
apply_gs org.gnome.desktop.interface color-scheme 'prefer-dark'
apply_gs org.gnome.desktop.interface gtk-theme 'Adwaita-dark'
apply_gs org.gnome.desktop.interface gtk-key-theme 'Emacs'
apply_gs org.gnome.desktop.interface icon-theme 'Adwaita'
apply_gs org.gnome.desktop.peripherals.touchpad tap-to-click true
apply_gs org.gnome.desktop.peripherals.touchpad natural-scroll true

if ! grep -q "MOZ_ENABLE_WAYLAND" ~/.bashrc; then
    {
      echo 'export MOZ_ENABLE_WAYLAND=1'
      echo 'export XDG_CURRENT_DESKTOP=sway'
    } >> ~/.bashrc
fi

echo "Done! Please reboot."
