#! /bin/sh

# Load the Xresources
if [ -d $HOME/.dotfiles/Xresources ]; then
    cat ~/.dotfiles/Xresources/* | xrdb
fi

# Set the background
if [ -d $HOME/images/wallpapers ] && [ `command -v xloadimage` ]; then
    random_bg=`find $HOME/images/wallpapers -type f | sort -R | head -n1`
    xloadimage -onroot -center -border black $random_bg
else
    xsetroot -solid steelblue
fi

# Starts a window manager
xmonad
