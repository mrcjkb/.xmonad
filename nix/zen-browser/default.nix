{
  inputs,
  pkgs,
  ...
}: let
  system = pkgs.stdenv.hostPlatform.system;
in {
  environment.systemPackages = [
    (inputs.zen-browser.packages."${system}".default.override {
      policies = let
        mkLockedAttrs = builtins.mapAttrs (_: value: {
          Value = value;
          Status = "locked";
        });
        mkExtensionSettings = builtins.mapAttrs (_: pluginId: {
          install_url = "https://addons.mozilla.org/firefox/downloads/latest/${pluginId}/latest.xpi";
          installation_mode = "force_installed";
        });
      in {
        AutofillAddressEnabled = true;
        AutofillCreditCardEnabled = false;
        DisableAppUpdate = true;
        DisableFeedbackCommands = true;
        DisableFirefoxStudies = true;
        DisablePocket = true;
        DisableTelemetry = true;
        DontCheckDefaultBrowser = true;
        NoDefaultBookmarks = true;
        OfferToSaveLogins = false;
        EnableTrackingProtection = {
          Value = true;
          Locked = true;
          Cryptomining = true;
          Fingerprinting = true;
        };
        Preferences = mkLockedAttrs {
          "browser.tabs.warnOnClose" = false;
        };
        ExtensionSettings = mkExtensionSettings {
          "adnauseam@rednoise.org" = "adnauseam";
          "gdpr@cavi.au.dk" = "consent-o-matic";
          "{05a4ff46-7c82-4e9d-832c-a6b8554a59c0}" = "hoogle-search";
          "jid1-KKzOGWgsW3Ao4Q@jetpack" = "i-dont-care-about-cookies";
          "search@kagi.com" = "kagi-search-for-firefox";
          "keepassxc-browser@keepassxc.org" = "keepassxc-browser";
          "{9fbd4423-3cfd-437b-be0e-53d5939c14ed}" = "materialtheme";
          "{a4c4eda4-fb84-4a84-b4a1-f7c1cbf2a1ad}" = "refined-github-";
        };
      };
    })
  ];
}
