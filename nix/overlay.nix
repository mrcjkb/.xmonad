final: prev:
with final.lib;
with final.haskell.lib;
{
   haskellPackages = prev.haskellPackages.override (old: {
    overrides = composeExtensions (old.overrides or (_: _: { })) (
      self: super:
        {
          xmobar = self.callPackage ./xmobar-app.nix;
        }
      );
   });
}
