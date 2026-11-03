# MX-Emacs package management

MX-Emacs manages the packages listed in `mx-packages-selected` in
`packages.el`. The shell wrapper and the corresponding interactive Emacs
commands provide the following operations.

## Check package status

Run:

```sh
./mx-emacs.sh status
```

Or, from Emacs, run `M-x mx-packages-status`.

The status report shows:

- The number of packages selected by the configuration.
- The number of packages installed in the local package directory.
- Configured packages that are missing.
- Installed packages that are not selected by the configuration.

Missing and unselected packages are listed by name. If configured packages are
missing, the report recommends running `./mx-emacs.sh init` (or
`M-x mx-packages-init`) to install them.

Status is read-only: it initializes Emacs' local package metadata to inspect
the installed set, but does not refresh or contact package archives, install or
remove packages, or otherwise change the installed package set.

## Manage packages

| Command | Purpose |
| --- | --- |
| `./mx-emacs.sh init` | Refresh package metadata and install missing configured packages. |
| `./mx-emacs.sh sync` | Refresh package metadata and upgrade installed packages. |
| `./mx-emacs.sh status` | Report package status without changing the installed set. |
| `./mx-emacs.sh clean` | Preview and interactively confirm removal of unselected packages. |
| `./mx-emacs.sh clean --yes` | Remove unselected packages without an interactive prompt. |

The corresponding Emacs commands are `M-x mx-packages-init`,
`M-x mx-packages-sync`, `M-x mx-packages-status`, and
`M-x mx-packages-clean`.

Only add packages to or remove packages from `mx-packages-selected` in
`packages.el` to change the configuration's declared package set.
