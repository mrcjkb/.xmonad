{
  pkgs ? import <nixpkgs> {system = builtins.currentSystem;},
  mkDerivation ? pkgs.mkDerivation,
  base ? pkgs.base,
  containers ? pkgs.containers,
  lib ? pkgs.lib,
  X11 ? pkgs.X11,
  xmonad ? pkgs.xmonad,
  xmonad-contrib ? pkgs.xmonad-contrib,
}:
mkDerivation {
  pname = "xmonadrc";
  version = "1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    containers
    X11
    xmonad
    xmonad-contrib
  ];
  executableHaskellDepends = [base xmonad xmonad-contrib];
  description = "My XMonad setup";
  license = lib.licenses.gpl2Only;
  mainProgram = "xmonadrc";
}
