#!/bin/bash

# Relevant to use the appropriate settings daemon. Supported values are:
# - "cinnamon"
# - "gnome"
# - "xfce"
# As a fallback, xsettingsd is used
desktop_environment="xfce"

# An associative array of identifiers to sets of themes. Themes are sperated
# by `;` and describe a type of theme based on their position:
# - position 1: gtk-Theme (ls /usr/share/themes/)
# - position 3: gtk-icon-theme (ls /usr/share/icons/)
# - position 3: emacs-Theme (M-x load-theme)
declare -A themesets
themesets=(
    ["light1"]="Greybird;Pocillo;modus-operandi;xterm-light"
    ["dark1"]="Adwaita-dark;Pocillo;modus-vivendi;xterm-dark"
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

    _switch_theme_gtk "$gtk_theme" "$gtk_icon_theme"
    _switch_theme_emacs "$emacs_theme"
    _switch_theme_terminal "$terminal_theme"
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
    if [ $(pgrep emacs) ]; then
        echo "switching to emacs theme: $1"
        emacsclient \
            --no-wait --eval \
            "(mapc #'disable-theme custom-enabled-themes)" \
            "(load-theme '$1)" > /dev/null
    else
        echo "skipping emacs, no server found"
    fi
}

# Switch themes of running terminals using escape sequences.
_switch_theme_terminal() {
    theme_dir="$HOME/.local/share/terminal_colors"
    ln -sf "$theme_dir/$1.conf" "$theme_dir/active.conf"

    echo "switching to terminal theme: $1"
    change-theme-terminal.sh $1
}

_sed() {
    sed --follow-symlinks -i "$@"
}

chosen_theme=$(echo "${!themesets[@]}" | sed 's/ /\n/g' | dmenu -l 10)
if [ "$chosen_theme" = "" ]; then
    echo "no theme chosen, exiting";
    exit 0
fi

_switch_theme $chosen_theme
