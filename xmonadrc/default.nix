{
  pkgs ? import <nixpkgs> {system = builtins.currentSystem;},
  lib ? pkgs.lib,
  haskellPackages ? pkgs.haskellPackages,
  base ? haskellPackages.base,
  containers ? haskellPackages.containers,
  X11 ? haskellPackages.X11,
  xmonad ? haskellPackages.xmonad,
  xmonad-contrib ? haskellPackages.xmonad-contrib,
}:
haskellPackages.mkDerivation {
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
