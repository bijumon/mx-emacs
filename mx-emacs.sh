#!/usr/bin/env bash
set -euo pipefail

CONFIG_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
EMACS="${EMACS:-emacs}"
COMMAND="${1:-}"
ASSUME_YES=0

if [[ "$COMMAND" == "clean" && "${2:-}" == "--yes" ]]; then
  ASSUME_YES=1
  shift 2
else
  shift || true
fi

if [[ $# -ne 0 ]]; then
  printf 'Usage: %s {init|sync|status|clean [--yes]}\n' "$0" >&2
  exit 2
fi

case "$COMMAND" in
  init|sync|status|clean) ;;
  *)
    printf 'Usage: %s {init|sync|clean [--yes]}\n' "$0" >&2
    exit 2
    ;;
esac

# Load only the package-management definitions. Loading init.el here would
# load post-init.el before missing packages have been installed.
if [[ "$COMMAND" == "clean" && "$ASSUME_YES" -eq 0 ]]; then
  # Cleanup needs a real terminal so the user can review and confirm removals.
  exec "$EMACS" -Q --no-window-system \
    --eval "(setq user-emacs-directory (file-name-as-directory \"$CONFIG_DIR\"))" \
    -l "$CONFIG_DIR/early-init.el" \
    --eval "(mx-packages-clean)"
elif [[ "$COMMAND" == "clean" ]]; then
  CLEAN_EXPRESSION="(mx-packages-clean t)"
else
  CLEAN_EXPRESSION="(mx-packages-$COMMAND)"
fi

exec "$EMACS" --batch -Q \
  --eval "(setq user-emacs-directory (file-name-as-directory \"$CONFIG_DIR\"))" \
  -l "$CONFIG_DIR/early-init.el" \
  --eval "$CLEAN_EXPRESSION"
