#!/bin/bash
# Change colors in xresources. I could try live reloading akin to
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

cat << EOF > $HOME/.local/share/terminal_colors/active_xrdb.conf

*background:   $(_hex_from_color_file background)
*foreground:   $(_hex_from_color_file foreground)
*.color0:  $(_hex_from_color_file color0)
*.color1:  $(_hex_from_color_file color1)
*.color2:  $(_hex_from_color_file color2)
*.color3:  $(_hex_from_color_file color3)
*.color4:  $(_hex_from_color_file color4)
*.color5:  $(_hex_from_color_file color5)
*.color6:  $(_hex_from_color_file color6)
*.color7:  $(_hex_from_color_file color7)
*.color8:  $(_hex_from_color_file color8)
*.color9:  $(_hex_from_color_file color9)
*.color10: $(_hex_from_color_file color10)
*.color11: $(_hex_from_color_file color11)
*.color12: $(_hex_from_color_file color12)
*.color13: $(_hex_from_color_file color13)
*.color14: $(_hex_from_color_file color14)

EOF

xrdb ~/.Xdefaults
