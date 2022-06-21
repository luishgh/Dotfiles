#!/bin/sh
# The abbreviated weekday (e.g., "Sat"), followed by the ISO-formatted date
# like 2018-10-06 and the time (e.g., 14:01)
date_formatted=$(date "+%a %d-%m-%y %H:%M")

# Returns the battery percentage
battery_status=$(cat /sys/class/power_supply/BAT0/capacity)

# Emojis and characters for the status bar
# 💎 💻 💡 🔌 ⚡ 📁 \|
echo 🔋 $battery_status 🕒 $date_formatted
