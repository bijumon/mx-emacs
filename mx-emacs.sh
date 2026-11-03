#!/usr/bin/env bash
set -euo pipefail

CONFIG_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
EMACS="${EMACS:-emacs}"
COMMAND="${1:-}"
ASSUME_YES=0

if [[ "${2:-}" == "--yes" && ( "$COMMAND" == "clean" || "$COMMAND" == "sync" ) ]]; then
  ASSUME_YES=1
  shift 2
else
  shift || true
fi

if [[ $# -ne 0 ]]; then
  printf 'Usage: %s {init|sync|clean [--yes]}\n' "$0" >&2
  exit 2
fi

case "$COMMAND" in
  init|sync|clean) ;;
  *)
    printf 'Usage: %s {init|sync|clean [--yes]}\n' "$0" >&2
    exit 2
    ;;
esac

# Load only the package-management definitions. Loading init.el here would
# load post-init.el before missing packages have been installed.
if [[ ( "$COMMAND" == "clean" || "$COMMAND" == "sync" ) && "$ASSUME_YES" -eq 0 ]]; then
  # Both operations need a real terminal so the user can review and confirm.
  exec "$EMACS" -Q --no-window-system \
    --eval "(setq user-emacs-directory (file-name-as-directory \"$CONFIG_DIR\"))" \
    -l "$CONFIG_DIR/early-init.el" \
    --eval "(mx-packages-$COMMAND)"
elif [[ "$COMMAND" == "clean" ]]; then
  COMMAND_EXPRESSION="(mx-packages-clean t)"
elif [[ "$COMMAND" == "sync" ]]; then
  COMMAND_EXPRESSION="(mx-packages-sync t)"
else
  COMMAND_EXPRESSION="(mx-packages-$COMMAND)"
fi

exec "$EMACS" --batch -Q \
  --eval "(setq user-emacs-directory (file-name-as-directory \"$CONFIG_DIR\"))" \
  -l "$CONFIG_DIR/early-init.el" \
  --eval "$COMMAND_EXPRESSION"
