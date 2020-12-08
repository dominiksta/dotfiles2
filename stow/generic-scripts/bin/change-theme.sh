#!/bin/bash

# Relevant to use the appropriate settings daemon. Supported values are:
# - "cinnamon"
# - "gnome"
# - "xfce"
# As a fallback, xsettingsd is used
desktop_environment=""

# An associative array of identifiers to sets of themes. Themes are sperated
# by `;` and describe a type of theme based on their position:
# - position 1: gtk-Theme (ls /usr/share/themes/)
# - position 3: gtk-icon-theme (ls /usr/share/icons/)
# - position 3: emacs-Theme (M-x load-theme)
# - position 4: terminal theme (ls ~/.local/share/terminal_colors/ or _emacs)
declare -A themesets
themesets=(
    ["light"]="Adwaita;Adwaita;gruvbox-light-soft;_emacs;Materia-light"
    ["dark"]="Adwaita-dark;Adwaita;gruvbox-dark-soft;_emacs;Materia-dark"
    # ["light"]="Adwaita;Adwaita;solarized-light;_emacs;Materia-light"
    # ["dark"]="Adwaita-dark;Adwaita;solarized-dark;_emacs;Materia-dark"
    # ["light"]="Adwaita;Adwaita;modus-operandi;_emacs;Materia-light"
    # ["dark"]="Adwaita-dark;Adwaita;modus-vivendi;_emacs;Materia-dark"
    # ["light"]="Materia-light-compact;Adwaita;modus-operandi;_emacs;Materia-light"
    # ["dark"]="Materia-dark-compact;Adwaita;modus-vivendi;_emacs;Materia-dark"
    # ["light"]="Mint-Y-Aqua;Mint-Y-Aqua;modus-operandi;_emacs;Mint-Y-Teal"
    # ["dark"]="Mint-Y-Dark-Aqua;Mint-Y-Dark-Aqua;modus-vivendi;_emacs;Mint-Y-Dark-Teal"
)

# Switch to a themeset specified in global `themesets`. Calls all the
# `_switch_theme_*`-commands.
_switch_theme() {
    declare -a THEMES
    IFS=';' read -ra THEMES <<< "${themesets[$1]}"
    gtk_theme="${THEMES[0]}"
    gtk_icon_theme="${THEMES[1]}"
    emacs_theme="${THEMES[2]}"
    terminal_theme="${THEMES[3]}"
    wm_theme="${THEMES[4]}"

    _switch_theme_gtk "$gtk_theme" "$gtk_icon_theme"
    _switch_theme_emacs "$emacs_theme"
    _switch_theme_xrdb "$terminal_theme"
    _switch_theme_tmux_main "$terminal_theme"
    _switch_theme_wm "$wm_theme"
}

_switch_theme_wm() {
    if pidof xfwm4; then
            echo "switching to xfwm theme: $1 (with xfsettingsd/xfconf-query)"
            xfconf-query -c xfwm4 -p /general/theme -s $1
    else
            echo "skipping setting wm theme, unsupported"
    fi
}

_switch_theme_gtk() {
    case $desktop_environment in

        cinnamon)
            echo "switching to gtk theme: $1 (with cinnamon-settings-daemon)"
            gsettings set org.cinnamon.desktop.interface gtk-theme $1
            gsettings set org.cinnamon.desktop.interface icon-theme $2
            ;;

        gnome)
            echo "switching to gtk theme: $1 (with gnome-settings-daemon)"
            gsettings set org.gnome.desktop.interface gtk-theme $1
            gsettings set org.gnome.desktop.interface icon-theme $2
            ;;

        xfce)
            echo "switching to gtk theme: $1 (with xfsettingsd/xfconf-query)"
            xfconf-query -c xsettings -p /Net/ThemeName -s $1
            xfconf-query -c xsettings -p /Net/IconThemeName -s $2
            ;;

        *)
            if [ ! $(which xsettingsd) ]; then
                echo "skipping gtk, no supported settings daemon found";
                return
            fi

            echo "switching to gtk theme: $1 (with xsettingsd)"
            xsettingsd_config="$HOME/.config/xsettingsd/xsettingsd.conf"

            _sed "s,\(.*\/ThemeName\) \"[a-zA-Z-]*\",\1 \"${1}\"," \
                 "$xsettingsd_config"
            _sed "s,\(.*\/IconThemeName\) \"[a-zA-Z-]*\",\1 \"${2}\"," \
                 "$xsettingsd_config"

            killall xsettingsd
            start-stop-daemon --start xsettingsd \
                              --exec $(which xsettingsd) --background
            ;;
    esac
}

# Change emacs theme to `$1` (if an emacs-server is running). Disables all other
# themes.
_switch_theme_emacs() {
    if pidof emacs; then
        echo "switching to emacs theme: $1"
        emacsclient \
            --no-wait --eval \
            "(mapc #'disable-theme custom-enabled-themes)" \
            "(load-theme '$1)" > /dev/null
    else
        echo "skipping emacs, no server found"
    fi

}

# Writes the theme file in $1 to ~/.local/share/terminal_colors/active.conf. If
# $1 is '_emacs', it will get the terminal colors from the current emacs theme
# instead.
_generate_theme_terminal() {
    if [ $1 == "_emacs" ]; then
        if pidof emacs; then
            cat << EOF > ~/.local/share/terminal_colors/active.conf
cursor=$(emacsclient --eval "(face-foreground 'default)")
foreground=$(emacsclient --eval "(face-foreground 'default)")
background=$(emacsclient --eval "(face-background 'default)")
color0=$(emacsclient --eval "(cdr (aref ansi-color-map 30))")
color8=$(emacsclient --eval "(cdr (aref ansi-color-map 40))")
color1=$(emacsclient --eval "(cdr (aref ansi-color-map 31))")
color9=$(emacsclient --eval "(cdr (aref ansi-color-map 41))")
color2=$(emacsclient --eval "(cdr (aref ansi-color-map 32))")
color10=$(emacsclient --eval "(cdr (aref ansi-color-map 42))")
color3=$(emacsclient --eval "(cdr (aref ansi-color-map 33))")
color11=$(emacsclient --eval "(cdr (aref ansi-color-map 43))")
color4=$(emacsclient --eval "(cdr (aref ansi-color-map 34))")
color12=$(emacsclient --eval "(cdr (aref ansi-color-map 44))")
color5=$(emacsclient --eval "(cdr (aref ansi-color-map 35))")
color13=$(emacsclient --eval "(cdr (aref ansi-color-map 45))")
color6=$(emacsclient --eval "(cdr (aref ansi-color-map 36))")
color14=$(emacsclient --eval "(cdr (aref ansi-color-map 46))")
color7=$(emacsclient --eval "(cdr (aref ansi-color-map 37))")
color15=$(emacsclient --eval "(cdr (aref ansi-color-map 47))")
EOF
        else
            echo "emacs is not running,"
            echo "falling back on xterm-dark for terminal colors"
            theme_dir="$HOME/.local/share/terminal_colors"
            cp -f "$theme_dir/xterm-dark.conf" "$theme_dir/active.conf"
        fi
    else
        theme_dir="$HOME/.local/share/terminal_colors"
        cp -f "$theme_dir/$1.conf" "$theme_dir/active.conf"
    fi
}


# Switch xrdb colors
_switch_theme_tmux_main() {
    echo "switching to terminal theme (main tmux session): $1"
    _generate_theme_terminal $1
    change-theme-terminal.sh
}

# Switch xrdb colors
_switch_theme_xrdb() {
    echo "switching to terminal theme (xrdb): $1"
    _generate_theme_terminal $1
    change-theme-xrdb.sh
}

_sed() {
    sed --follow-symlinks -i "$@"
}

if [ -z "$1" ]; then
    chosen_theme=$(echo "${!themesets[@]}" | sed 's/ /\n/g' | dmenu -l 10)
    if [ "$chosen_theme" = "" ]; then
        echo "no theme chosen, exiting";
        exit 0
    fi
else
    chosen_theme=$1
fi

_switch_theme $chosen_theme
