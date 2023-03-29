{
  description = "NixOS module for an xmonad session";

  nixConfig = {
    extra-substituters = "https://mrcjkb.cachix.org";
    extra-trusted-public-keys = "mrcjkb.cachix.org-1:KhpstvH5GfsuEFOSyGjSTjng8oDecEds7rbrI96tjA4=";
  };

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    nixpkgs,
    haskellNix,
    pre-commit-hooks,
    ...
  }: let
    supportedSystems = [
      "aarch64-linux"
      "x86_64-linux"
    ];
    perSystem = nixpkgs.lib.genAttrs supportedSystems;
    pre-commit-check-for = system:
      pre-commit-hooks.lib.${system}.run {
        src = ./.;
        hooks = {
          alejandra.enable = true;
        };
      };

    compiler-nix-name = "ghc927";
    overlays = [
      haskellNix.overlay
      (final: prev: {
        xmonadrc = final.haskell-nix.cabalProject' {
          src = builtins.path {
            path = ./.;
            name = "xmonadrc";
          };
          inherit compiler-nix-name;
          # This is used by `nix develop .` to open a shell for use with
          # `cabal`, `hlint` and `haskell-language-server`
          shell = {
            name = "xmonadrc-shell";
            tools = {
              cabal = "latest";
              hlint = "latest";
              haskell-language-server = "latest";
            };
            withHoogle = true;
            packages = with pkgs.haskellPackages; [
              implicit-hie
            ];
            shellHook = ''
              gen-hie --cabal hie.yaml
            '';
          };
        };
        xmobar-app = final.haskell-nix.cabalProject' {
          src = builtins.path {
            path = ./xmonad-session/xmobar;
            name = "xmobar-app";
          };
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
            shellHook = ''
              pushd xmonad-session/xmobar
              gen-hie --cabal hie.yaml
              popd
            '';
          };
        };
      })
    ];
    pkgs = import nixpkgs {
      system = "x86_64-linux";
      inherit overlays;
      inherit (haskellNix) config;
    };
    xmonadrc-flake = pkgs.xmonadrc.flake {};
    xmobar-app-flake = pkgs.xmobar-app.flake {};
    merged-flakes = pkgs.lib.attrsets.recursiveUpdate xmonadrc-flake xmobar-app-flake;
    xmonadrc-package = xmonadrc-flake.packages."xmonadrc:exe:xmonadrc";
    xmobar-package = xmobar-app-flake.packages."xmobar-app:exe:xmobar-app";
  in
    pkgs.lib.attrsets.recursiveUpdate
    merged-flakes
    {
      nixosModule = import ./xmonad-session;
      defaultPackage.x86_64-linux = xmonadrc-package;
      inherit xmonadrc-package xmobar-package;
      checks = perSystem (system: {
        formatting = pre-commit-check-for system;
      });
    };
}
