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

ansi_sequences=""

# Prepare ANSI sequences for colors 0-15.
for i in {0..15}; do
    ansi_sequences+="\\033]4;${i};$(_hex_from_color_file color"${i}")\\007"
done

# Prepare ANSI sequences for foreground, background, cursor
ansi_sequences+="\\033]10;$(_hex_from_color_file foreground)\\007"
ansi_sequences+="\\033]11;$(_hex_from_color_file background)\\007"
ansi_sequences+="\\033]17;$(_hex_from_color_file cursor)\\007"

# Apply ANSI sequences to running terminals.
for term in /dev/pts/[0-9]*; do
    printf "%b" "$ansi_sequences" > "$term" &
done
