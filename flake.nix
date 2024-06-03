{
  description = "NixOS module for an xmonad session";

  nixConfig = {
    extra-substituters = "https://mrcjkb.cachix.org";
    extra-trusted-public-keys = "mrcjkb.cachix.org-1:KhpstvH5GfsuEFOSyGjSTjng8oDecEds7rbrI96tjA4=";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";
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

    overlay = import ./nix/overlay.nix {};
  in
    flake-utils.lib.eachSystem supportedSystems (system: let
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
          ++ self.checks.${system}.pre-commit-check.enabledPackages;
        shellHook = ''
          ${self.checks.${system}.pre-commit-check.shellHook}
          gen-hie --cabal > hie.yaml
        '';
      };
    in {
      devShells = {
        default = shell;
      };
      packages = rec {
        default = xmobar-app;
        xmobar-app = pkgs.haskellPackages.xmobar-app;
      };
      checks = {
        inherit pre-commit-check;
      };
    })
    // {
      nixosModules.default = {...}: {
        imports = [
          ./nix/xmonad-session
        ];
        nixpkgs.overlays = [
          overlay
        ];
      };
    };
}
