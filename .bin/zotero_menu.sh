#!/usr/bin/env bash

# Query titles + PDF paths
entries=$(find "$HOME/Documents/Zotero/storage/" -name '*.pdf')

# Prepare dmenu list
choice=$(echo "$entries" | xargs -d '\n' -i basename {} | bemenu -i -p "Zotero PDF:")
echo "CHOICE: $choice"

[ -z "$choice" ] && exit 0

# Find PDF path for chosen title
pdf_path=$(echo "$entries" | grep "$choice")

echo "FINAL: $pdf_path"

# Open it
open "$pdf_path" >/dev/null 2>&1 &

