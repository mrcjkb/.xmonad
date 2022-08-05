{
  description = "NixOS module for an xmonad session";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };

  outputs = {self, nixpkgs, haskellNix, ...}:
  let
  overlays = [ 
    haskellNix.overlay
    (final: prev: {
      xmonadrc = final.haskell-nix.cabalProject' {
        src = ./.;
        compiler-nix-name = "ghc8107";
        # This is used by `nix develop .` to open a shell for use with
        # `cabal`, `hlint` and `haskell-language-server`
        shell = {
          tools = {
            cabal = "latest";
            hlint = "latest";
            haskell-language-server = "latest";
          };
          withHoogle = true;
          # FIXME: This seems to be broken in haskell.nix
          # buildInputs = with pkgs.haskellPackages; [
          #   implicit-hie
          # ];
          # shellHook = ''
          #   gen-hie --cabal hie.yaml
          # '';
        };
      };
    })
  ];
  pkgs = import nixpkgs { system = "x86_64-linux"; inherit overlays; inherit (haskellNix) config; };
  xmonadrc-flake = pkgs.xmonadrc.flake {};
  in xmonadrc-flake // {
    nixosModule = import ./xmonad-session;
    defaultPackage.x86_64-linux = xmonadrc-flake.packages."xmonadrc:exe:xmonadrc";
  };
}
