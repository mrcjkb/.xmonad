{}: final: prev:
with final.haskell.lib;
with final.lib; let
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: {})) (
      self: super: let
        xmonadrc = buildFromSdist (self.callPackage ../xmonadrc/default.nix {});
        xmobar-app = buildFromSdist (self.callPackage ../xmobar-app/default.nix {});
      in {
        inherit
          xmonadrc
          xmobar-app
          ;
      }
    );
  });
in {
  inherit haskellPackages;
}
