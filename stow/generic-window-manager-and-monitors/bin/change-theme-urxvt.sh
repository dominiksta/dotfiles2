#!/bin/bash
# Switch themes of running terminals using escape sequences.
# Adapted from
# https://gitlab.com/protesilaos/dotfiles/-/blob/master/bin/bin/repaint_terminals

active_theme="$HOME/.local/share/terminal_colors/active.conf"

if [ -f "$active_theme" ]; then
    # capture all active colours
    color_array="$(grep '^.*=' $active_theme)"
else
    echo "ERROR. There is no file at $active_theme"
    exit 1
fi

# Transform lines like `colorX="#123456"` to `#123456`
_hex_from_color_file() {
    # NOTE we pass `-w` to `grep` to make sure it matches only whole words.
    # Otherwise a 'foreground' would also capture 'foregroundalt'.
    echo "$color_array" | grep -w "$1" | sed 's/\(.*\)"\(#[a-zA-Z0-9]*\)"/\2/'
}

cat << EOF > $HOME/.local/share/terminal_colors/active_urxvt.conf

*background:   $(_hex_from_color_file background)
*foreground:   $(_hex_from_color_file foreground)
URxvt.color0:  $(_hex_from_color_file color0)
URxvt.color1:  $(_hex_from_color_file color1)
URxvt.color2:  $(_hex_from_color_file color2)
URxvt.color3:  $(_hex_from_color_file color3)
URxvt.color4:  $(_hex_from_color_file color4)
URxvt.color5:  $(_hex_from_color_file color5)
URxvt.color6:  $(_hex_from_color_file color6)
URxvt.color7:  $(_hex_from_color_file color7)
URxvt.color8:  $(_hex_from_color_file color8)
URxvt.color9:  $(_hex_from_color_file color9)
URxvt.color10: $(_hex_from_color_file color10)
URxvt.color11: $(_hex_from_color_file color11)
URxvt.color12: $(_hex_from_color_file color12)
URxvt.color13: $(_hex_from_color_file color13)
URxvt.color14: $(_hex_from_color_file color14)

EOF

xrdb ~/.Xdefaults
kill -1 $(pidof urxvtd)
