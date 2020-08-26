#!/usr/bin/env sh

METADATA=$(timeout 1s playerctl metadata --format '{{lc(status)}};{{artist}};{{title}}')

STATUS=$(echo "$METADATA" | cut -d';' -f1)

if [ "$STATUS" == "No player could handle this command" ]; then
  echo -n ""
  exit 1
fi

if [ "$STATUS" == "playing" ]; then
  ARTIST=$(echo "$METADATA" | cut -d';' -f2)
  TITLE=$(echo "$METADATA" | cut -d';' -f3)

  echo "$ARTIST: $TITLE"
else
  echo ""
fi
