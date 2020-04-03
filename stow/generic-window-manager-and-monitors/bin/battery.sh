#!/bin/sh

# Prints all batteries, their percentage remaining and charge status. Adapted from
# https://github.com/LukeSmithxyz/voidrice/blob/master/.local/bin/statusbar/battery.

# Loop through all attached batteries.
for battery in /sys/class/power_supply/BAT?
do
	# Get its remaining capacity and charge status.
	capacity=$(cat "$battery"/capacity) || break
	status=$(sed "s/Discharging/↓/;s/Not charging/↓/;s/Charging/↑/;s/Unknown/?/;s/Full/-/" "$battery"/status)

	# If it is discharging and 25% or less, we will add a ❗ as a warning.
	 [ "$capacity" -le 25 ] && [ "$status" = "↓" ] && warn="!"

	printf "%s%s%s%%\n" "$status" "$warn" "$capacity"
	unset warn
done
