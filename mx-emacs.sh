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

# Batch-mode Emacs reads minibuffer input from stdin.  Enable confirmation
# only when this script is attached to a terminal; unattended jobs must use
# the explicit --yes option.
if [[ ( "$COMMAND" == "clean" || "$COMMAND" == "sync" ) &&
      "$ASSUME_YES" -eq 0 && -t 0 ]]; then
  export MX_EMACS_CONFIRM=1
fi

# Load only the package-management definitions. Loading init.el here would
# load post-init.el before missing packages have been installed.
if [[ "$COMMAND" == "clean" && "$ASSUME_YES" -eq 1 ]]; then
  COMMAND_EXPRESSION="(mx-packages-clean t)"
elif [[ "$COMMAND" == "sync" && "$ASSUME_YES" -eq 1 ]]; then
  COMMAND_EXPRESSION="(mx-packages-sync t)"
else
  COMMAND_EXPRESSION="(mx-packages-$COMMAND)"
fi

exec "$EMACS" --batch -Q \
  --eval "(setq user-emacs-directory (file-name-as-directory \"$CONFIG_DIR\"))" \
  -l "$CONFIG_DIR/early-init.el" \
  --eval "$COMMAND_EXPRESSION"
