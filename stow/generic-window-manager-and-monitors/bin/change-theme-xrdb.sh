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

XTerm*background:   $(_hex_from_color_file background)
XTerm*foreground:   $(_hex_from_color_file foreground)
XTerm*.color0:  $(_hex_from_color_file color0)
XTerm*.color1:  $(_hex_from_color_file color1)
XTerm*.color2:  $(_hex_from_color_file color2)
XTerm*.color3:  $(_hex_from_color_file color3)
XTerm*.color4:  $(_hex_from_color_file color4)
XTerm*.color5:  $(_hex_from_color_file color5)
XTerm*.color6:  $(_hex_from_color_file color6)
XTerm*.color7:  $(_hex_from_color_file color7)
XTerm*.color8:  $(_hex_from_color_file color8)
XTerm*.color9:  $(_hex_from_color_file color9)
XTerm*.color10: $(_hex_from_color_file color10)
XTerm*.color11: $(_hex_from_color_file color11)
XTerm*.color12: $(_hex_from_color_file color12)
XTerm*.color13: $(_hex_from_color_file color13)
XTerm*.color14: $(_hex_from_color_file color14)

EOF

xrdb ~/.Xdefaults
