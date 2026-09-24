# .emacs.d

Personal Emacs configuration for Emacs 29+ (developed on Emacs 31 / macOS).

## Layout

| File               | Purpose                                                     |
| ------------------ | ----------------------------------------------------------- |
| `early-init.el`    | Pre-GUI startup settings, macOS native-compilation fix      |
| `init.el`          | Loads the files below; sets `custom-file`                   |
| `init-packages.el` | Package archives (GNU, NonGNU, MELPA) and `use-package`     |
| `theme.el`         | Theme, modeline, icons, font                                |
| `settings.el`      | General editor settings, ivy/counsel, dired, vterm          |
| `programming.el`   | Language modes, magit, projectile, python (elpy)            |
| `custom.el`        | Personal helper functions, hydra menu and key bindings      |
| `django.el`        | Django hydra (`F4`)                                         |
| `custom-vars.el`   | Written by Customize (don't hand-edit)                      |
| `snippets/`        | yasnippet snippets                                          |

Packages are declared with `use-package` next to the code that configures them.

## First-time setup

1. Launch Emacs. Missing packages are installed automatically from the package
   archives on first start (needs network access); the first start is slow.
2. Icon fonts: run `M-x nerd-icons-install-fonts` once, then restart Emacs.
   If the icons still don't show (`(find-font (font-spec :family
   "Symbols Nerd Font Mono"))` returns nil), macOS hasn't registered the new
   font: run `killall fontd` in a shell (launchd restarts it and rescans
   `~/Library/Fonts`) and restart Emacs.
3. Terminal: `vterm` (`<f2>`) builds a native module the first time it is
   used, which needs `brew install cmake libvterm` (with the Homebrew
   `libvterm`, the build doesn't need GNU libtool).
4. Python: create a virtual environment for elpy at `~/.venv` (see
   `programming.el`). It needs Python >= 3.11 (elpy imports `tomllib`), which
   is newer than the Python bundled with the macOS command line tools, so use
   `uv` to get a current one:

   ```sh
   uv venv --python 3.13 ~/.venv
   uv pip install --python ~/.venv/bin/python jedi flake8 black ipython
   ```

   `M-x elpy-config` shows what is still missing.

## Key bindings

| Key           | Action                                                        |
| ------------- | ------------------------------------------------------------- |
| `<f2>`        | vterm for the current project (`C-u <f2>`: always a new one)  |
| `<f3>`        | Menu: projectile, git-link, python venv, ipython/jupyter, ... |
| `<f3> C`      | Claude Code in a vterm at the current project's root          |
| `<f3> L`      | Same, but resume the last conversation (`claude --continue`)  |
| `<f4>`        | Django menu (finds `manage.py` upward from the buffer)        |
| `C-s`         | `swiper-isearch`                                              |
| `C-.`         | `company-complete`                                            |
| `C-c p`       | projectile command map                                        |
| `C-;`         | Comment/uncomment line or region                              |
| `S-<arrows>`  | Move between windows                                          |
| `C-S-<arrows>`| Swap buffers between windows (buffer-move)                    |
| `C-c w [ { <` | Wrap in a pair (`insert-pair`)                                |

`<f3> C` needs the `claude` CLI on `PATH` in an interactive shell (GUI launches
get your shell's `PATH` via `exec-path-from-shell`). vterm buffers
rename themselves to `*vterm: <project>*` as the shell changes directory.

## Notes

- macOS native compilation: `early-init.el` sets `MACOSX_DEPLOYMENT_TARGET`
  because libgccjit otherwise passes an invalid `-mmacosx-version-min` to
  clang on new macOS releases, which broke startup.
- `emacs --batch` does not run `early-init.el` or activate packages. To test
  the config headlessly, start a daemon: `emacs --daemon=test --debug-init`.
