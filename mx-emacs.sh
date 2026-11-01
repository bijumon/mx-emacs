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

# Load the same early and regular configuration used by interactive Emacs.
# In batch mode, enable :ensure so use-package declarations install packages.
exec "$EMACS" --batch -Q \
  --eval "(setq user-emacs-directory (file-name-as-directory \"$CONFIG_DIR\"))" \
  -l "$CONFIG_DIR/early-init.el" \
  --eval '(setq use-package-always-ensure t)' \
  -l "$CONFIG_DIR/init.el" \
  --eval "(mx-packages-$COMMAND)"
