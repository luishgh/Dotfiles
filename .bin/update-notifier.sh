#!/bin/sh

# Gentoo
function get_last_sync_day {
    # TODO fix this
    cat /var/db/repos/gentoo/metadata/timestamp.chk | awk '{print $2}'
}

# Guix System
function get_last_pull_day {
    last_pull_day=$(guix describe | awk '/Generation/ {print $4 " " $3 " " $5}')
    date -d "$last_pull_day" +"%Y-%m-%d"
}

# Check current running distro
if [ -d "/etc/portage" ]; then
    last_day=$(get_last_sync_day)
else
    last_day=$(get_last_pull_day)
fi

today=$(date +%F)

if [[ "$last_day" < "$today" ]]; then
    echo ""
else
    echo ""
fi
