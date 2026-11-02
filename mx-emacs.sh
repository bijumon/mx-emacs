#!/usr/bin/env bash
set -euo pipefail

CONFIG_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
EMACS="${EMACS:-emacs}"
COMMAND="${1:-}"

case "$COMMAND" in
  init|sync|clean) ;;
  *)
    printf 'Usage: %s {init|sync|clean}\n' "$0" >&2
    exit 2
    ;;
esac

# Load the normal configuration. Startup is offline; the selected command
# performs package changes explicitly after configuration has loaded.
exec "$EMACS" --batch -Q \
  --eval "(setq user-emacs-directory (file-name-as-directory \"$CONFIG_DIR\"))" \
  -l "$CONFIG_DIR/early-init.el" \
  -l "$CONFIG_DIR/init.el" \
  --eval "(mx-packages-$COMMAND)"
