{
  description = "NixOS module for an xmonad session";

  outputs = {self, ...}:
  {
    nixosModule = import ./xmonad-session;
  };
}
