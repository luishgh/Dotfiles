#!/usr/bin/env bash

ZOTERO_DB="$HOME/Documents/Zotero/zotero.sqlite"
ZOTERO_STORAGE="$HOME/Documents/Zotero/storage/"

# Query titles + PDF paths
entries=$(sqlite3 -separator '|' "$ZOTERO_DB" "
SELECT itemDataValues.value AS title, itemAttachments.path
FROM itemAttachments
JOIN items parent ON itemAttachments.parentItemID = parent.itemID
JOIN itemData ON parent.itemID = itemData.itemID
JOIN fields ON itemData.fieldID = fields.fieldID
JOIN itemDataValues ON itemData.valueID = itemDataValues.valueID
WHERE fields.fieldName = 'title'
  AND itemAttachments.path LIKE 'storage:%pdf';
")

# Prepare dmenu list
choice=$(echo "$entries" | cut -d'|' -f1 | bemenu -i -p "Zotero PDF:")

[ -z "$choice" ] && exit 0

# Find PDF path for chosen title
pdf_path=$(echo "$entries" | grep "^$choice|" | cut -d'|' -f2 | sed 's#storage:##')

echo "$pdf_path"

final_path=$(find "$ZOTERO_STORAGE" -type f -iname "$pdf_path" | head -n 1)

echo "$final_path"

# Open it
open "$final_path" >/dev/null 2>&1 &

