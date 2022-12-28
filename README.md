# My XMonad config.

[![Nix build](https://github.com/MrcJkb/.xmonad/actions/workflows/nix-build.yml/badge.svg)](https://github.com/MrcJkb/.xmonad/actions/workflows/nix-build.yml)

See also the corresponding [NixOS flake](https://github.com/MrcJkb/nixfiles/blob/master/flake.nix).

## To enter a development shell (with haskell-language-server)

Nix needs to be installed with flakes enabled.

To enter a nix shell for the `xmonadrc`

```
nix develop .
```

To enter a nix shell for the `xmobar-app`:

```
nix develop .#xmobar-package
```
