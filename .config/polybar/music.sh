#!/usr/bin/env sh

METADATA=$(timeout 1s playerctl metadata --format '{{lc(status)}};{{artist}};{{title}}' 2>/dev/null)

if [ "$?" -ne 0 ]; then
  echo ""
  exit 1
fi

STATUS=$(echo "$METADATA" | cut -d';' -f1)

if [ "$STATUS" = "playing" ]; then
  ARTIST=$(echo "$METADATA" | cut -d';' -f2)
  TITLE=$(echo "$METADATA" | cut -d';' -f3)

  echo "$ARTIST: $TITLE"
else
  echo ""
fi
