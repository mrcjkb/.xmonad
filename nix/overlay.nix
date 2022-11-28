final: prev:
with final.lib;
with final.haskell.lib;
{
   xmobar-app = self.callPackage ./xmobar-app.nix;

   haskellPackages = prev.haskellPackages.override (old: {
    overrides = composeExtensions (old.overrides or (_: _: { })) (
      self: super:
        {
          xmobar = xmobar-app;
        }
      );
   });
}
