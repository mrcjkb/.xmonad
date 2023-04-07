{
  description = "NixOS module for an xmonad session";

  nixConfig = {
    extra-substituters = "https://mrcjkb.cachix.org";
    extra-trusted-public-keys = "mrcjkb.cachix.org-1:KhpstvH5GfsuEFOSyGjSTjng8oDecEds7rbrI96tjA4=";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    pre-commit-hooks,
    flake-utils,
    ...
  }: let
    supportedSystems = [
      "aarch64-linux"
      "x86_64-linux"
    ];
  in
    flake-utils.lib.eachSystem supportedSystems (system: let
      overlay = import ./nix/overlay.nix {};

      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          overlay
        ];
      };

      pre-commit-check = pre-commit-hooks.lib.${system}.run {
        src = self;
        hooks = {
          alejandra = {
            enable = true;
            excludes = [
              "xmobar-app/default.nix"
              "xmonadrc/default.nix"
            ];
          };
          cabal2nix.enable = true;
          editorconfig-checker.enable = true;
          markdownlint.enable = true;
          fourmolu.enable = true;
          # hpack.enable = true;
          hlint.enable = true;
        };
      };

      shell = pkgs.haskellPackages.shellFor {
        name = "xmonadrc-devShell";
        packages = p:
          with p; [
            xmonadrc
            xmobar-app
          ];
        withHoogle = true;
        buildInputs =
          (with pkgs; [
            haskell-language-server
            cabal-install
            zlib
          ])
          ++ (with pkgs.haskellPackages; [
            implicit-hie
          ])
          ++ (with pre-commit-hooks.packages.${system}; [
            hlint
            hpack
            fourmolu
            cabal2nix
            alejandra
          ]);
        shellHook = ''
          ${self.checks.${system}.pre-commit-check.shellHook}
          gen-hie --cabal > hie.yaml
        '';
      };
    in {
      devShells = {
        default = shell;
      };
      nixosModules.default = import ./nix/xmonad-session;
      packages = rec {
        default = xmobar-app;
        xmobar-app = pkgs.haskellPackages.xmobar-app;
      };
      checks = {
        inherit pre-commit-check;
      };
    });
}
