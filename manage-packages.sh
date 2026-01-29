#!/usr/bin/env bash
# manage-packages.sh - frontend to manage Emacs packages using emacs --script
# Usage:
#   manage-packages.sh -c <install|remove|clean|update|list> -e <emacs-dir> [-p <packages-file>] [-b <emacs-binary>]
# Or set EMACSDIR in environment and call:
#   EMACSDIR=/path/to/emacs-dir ./manage-packages.sh -c install
#
# Behavior:
#  - Does NOT assume ~/.emacs.d. You must supply an emacs-dir via -e or EMACSDIR.
#  - If packages-file not provided, defaults to "$EMACSDIR/packages.el".
#  - Runs Emacs with --script on a temporary elisp file (cleaned up afterwards).

set -euo pipefail

progname="$(basename "$0")"
show_help() {
  cat <<EOF
Usage: $progname -c <cmd> -e <emacs-dir> [-p <packages-file>] [-b <emacs-binary>]
  -c <cmd>            Command: install | remove | clean | update | list
  -e <emacs-dir>      Path to your Emacs configuration directory (where manage-packages.el lives)
  -p <packages-file>  Optional path to packages.el (defaults to <emacs-dir>/packages.el)
  -b <emacs-binary>   Optional path to Emacs binary (defaults to 'emacs' from PATH)
  -h                  Show this help
Examples:
  $progname -c install -e /home/me/.emacs.d
  $progname -c update -e /path/to/dot-emacs -p /path/to/packages.el
  EMACSDIR=/home/me/.emacs.d $progname -c list
EOF
}

# defaults
EMACS_CMD="${EMACS_CMD:-emacs}"
CMD=""
PACKAGES_FILE=""
EMACSDIR="${EMACSDIR:-}"

# parse options
while getopts ":c:e:p:b:h" opt; do
  case "$opt" in
    c) CMD="$OPTARG" ;;
    e) EMACSDIR="$OPTARG" ;;
    p) PACKAGES_FILE="$OPTARG" ;;
    b) EMACS_CMD="$OPTARG" ;;
    h)
       show_help
       exit 0
       ;;
    \?)
       echo "Invalid option: -$OPTARG" >&2
       show_help
       exit 2
       ;;
    :)
       echo "Option -$OPTARG requires an argument." >&2
       show_help
       exit 2
       ;;
  esac
done

# minimal validation
if [ -z "$CMD" ]; then
  echo "Error: command required (-c)." >&2
  show_help
  exit 2
fi

if [ -z "${EMACSDIR:-}" ]; then
  echo "Error: Emacs directory not specified. Use -e <emacs-dir> or set EMACSDIR in environment." >&2
  exit 2
fi

# expand EMACSDIR
EMACSDIR="$(cd "$EMACSDIR" 2>/dev/null && pwd || echo "$EMACSDIR")"

if [ ! -d "$EMACSDIR" ]; then
  echo "Error: EMACSDIR '$EMACSDIR' is not a directory or does not exist." >&2
  exit 2
fi

# default packages file
if [ -z "$PACKAGES_FILE" ]; then
  PACKAGES_FILE="$EMACSDIR/packages.el"
fi

# create temp elisp runner in a portable way
if tmpfile=$(mktemp 2>/dev/null); then
  TMP_EL="$tmpfile.el"
  mv "$tmpfile" "$TMP_EL" 2>/dev/null || true
elif tmpfile=$(mktemp -t manage-packages 2>/dev/null); then
  TMP_EL="$tmpfile.el"
else
  TMP_EL="/tmp/manage-packages-$$.el"
fi

cleanup() {
  rm -f "$TMP_EL"
}
trap cleanup EXIT

# write elisp runner; it reads EMACSDIR, PACKAGES_FILE and CMD from env
cat > "$TMP_EL" <<'ELISP'
;; Temporary runner for manage-packages
;; Expects environment variables:
;;   EMACSDIR  - path to emacs dir containing manage-packages.el
;;   PACKAGES_FILE - optional path to packages.el
;;   CMD       - command: install|remove|clean|update|list
;; Temporary runner for manage-packages
(let* ((emacs-dir (file-name-as-directory (expand-file-name (getenv "EMACSDIR"))))
       (pkg-file (getenv "PACKAGES_FILE"))
       (cmd (getenv "CMD")))

  ;; Force Emacs to treat EMACSDIR as its config root
  (setq user-emacs-directory emacs-dir)
  (setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

  ;; Ensure directories exist
  (make-directory package-user-dir t)

  ;; Ensure correct load-path
  (add-to-list 'load-path user-emacs-directory)

  ;; Load package manager
  (load (expand-file-name "manage-packages.el" user-emacs-directory) nil 'nomessage)

  ;; Dispatch command
  (cond
   ((string= cmd "install")
    (packages-install pkg-file))
   ((string= cmd "remove")
    (packages-remove pkg-file))
   ((string= cmd "clean")
    (packages-clean))
   ((string= cmd "update")
    (packages-update pkg-file))
   ((string= cmd "list")
    (packages-list pkg-file))
   (t
    (princ (format "Unknown command: %s\n" cmd))
    (kill-emacs 3)))

  (kill-emacs 0))
ELISP

# export env vars for the elisp runner
export EMACSDIR
export PACKAGES_FILE
export CMD

# run emacs --script
"$EMACS_CMD" --script "$TMP_EL"
exit_code=$?

if [ "$exit_code" -ne 0 ]; then
  echo "manage-packages: Emacs exited with code $exit_code" >&2
fi

exit "$exit_code"

