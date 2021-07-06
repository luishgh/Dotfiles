#!/bin/sh
# Show pacman updates

updates=$(checkupdates | wc -l)
echo "$updates updates"
