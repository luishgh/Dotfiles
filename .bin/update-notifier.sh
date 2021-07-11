#!/bin/sh

# Gentoo
function get_last_sync_day {
    cat /var/db/repos/gentoo/metadata/timestamp.chk | awk '{print $2}'
}

# Guix
function get_last_pull_day {
    guix describe | grep Generation | awk '{print $4}'
}

# Check current running distro
if [ -d "/etc/portage" ]; then
    last_day=$(get_last_sync_day)
else
    last_day=$(get_last_pull_day)
fi

today=$(date | awk '{print $3}')

if ((last_day < today)); then
    echo ""
else
    echo ""
fi
