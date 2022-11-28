{
  description = "NixOS module for an xmonad session";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };

  outputs = {self, nixpkgs, haskellNix, ...}:
  let
  compiler-nix-name = "ghc925";
  overlays = [ 
    haskellNix.overlay
    (final: prev: {
      xmonadrc = final.haskell-nix.cabalProject' {
        src = builtins.path { path = ./.; name = "xmonadrc"; };
        inherit compiler-nix-name; 
        # This is used by `nix develop .` to open a shell for use with
        # `cabal`, `hlint` and `haskell-language-server`
        shell = {
          tools = {
            cabal = "latest";
            hlint = "latest";
            haskell-language-server = "latest";
          };
          withHoogle = true;
          packages = with pkgs.haskellPackages; [
            implicit-hie
          ];
          # FIXME
          # shellHook = ''
          #   gen-hie --cabal hie.yaml
          # '';
        };
      };
      xmobar-app = final.haskell-nix.cabalProject' {
        src = builtins.path { path = ./xmonad-session/xmobar; name = "xmobar-app"; };
        inherit compiler-nix-name; 
        # This is used by `nix develop .` to open a shell for use with
        # `cabal`, `hlint` and `haskell-language-server`
        shell = {
          tools = {
            cabal = "latest";
            hlint = "latest";
            haskell-language-server = "latest";
          };
          withHoogle = true;
        };
      };
    })
  ];
  pkgs = import nixpkgs { system = "x86_64-linux"; inherit overlays; inherit (haskellNix) config; };
  xmonadrc-flake = pkgs.xmonadrc.flake {};
  xmobar-app-flake = pkgs.xmobar-app.flake {};
  xmonadrc-package = xmonadrc-flake.packages."xmonadrc:exe:xmonadrc";
  xmobar-package = xmobar-app-flake.packages."xmobar-app:exe:xmobar-app";
  in xmonadrc-flake // {
    nixosModule = import ./xmonad-session;
    defaultPackage.x86_64-linux = xmonadrc-package;
    inherit xmonadrc-package xmobar-package;
  };
}
