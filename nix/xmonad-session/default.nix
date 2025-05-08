{
  pkgs,
  lib,
  defaultUser ? "mrcjk",
  ...
}: let
  xmonadrc = pkgs.haskellPackages.xmonadrc;
in {
  services.xserver.windowManager = {
    session = [
      {
        name = "xmonad";
        start = ''
          systemd-cat -t xmonad -- ${lib.getExe xmonadrc} &
          waitPID=$!
        '';
      }
    ];
  };
  home-manager.users."${defaultUser}" = {
    xdg.configFile."rofi" = {
      # TODO: use home-manager module
      source = ../../configs/rofi/.;
      recursive = true;
    };
    programs = {
      rofi = {
        enable = true;
      };
    };
    services = {
      dunst = {
        enable = true;
        iconTheme = {
          name = "Papirus-Dark";
          package = pkgs.papirus-icon-theme;
        };
      };
    };
  };

  services = {
    displayManager = {
      defaultSession = "none+xmonad";
    };
    xserver = {
      # Enable the X11 windowing system.
      enable = true;
      displayManager = {
        lightdm = {
          enable = true;
          greeters.mini = {
            enable = true;
            user = defaultUser;
            extraConfig = ''
              [greeter]
              show-password-label = false
              [greeter-theme]
              background-image = ""
            '';
          };
          extraConfig = ''
            xserver-command=X -maxbigreqsize 127
          '';
        };
      };
    };
    picom = {
      enable = true;
      activeOpacity = 1.0;
      inactiveOpacity = 1.0;
      backend = "glx";
      fade = false;
      shadow = false;
    };
    blueman.enable = true;
  };
  programs = {
    slock.enable = true;
  };
  environment = {
    systemPackages =
      (with pkgs.haskellPackages; [
        xmonad
        xmobar-app
        greenclip # Clipboard manager for use with rofi
      ])
      ++ (with pkgs; [
        ranger # TUI file browser
        alacritty
        dmenu # Expected by xmonad
        gxmessage # Used by xmonad to show help
        xorg.xkill # Kill X windows with the cursor
        pavucontrol # PulseAudio volume control UI
        brightnessctl # Brightness control CLI
        flameshot # A command-line screen capture utility
        pamixer # PulseAudio volume mixer
        pango # Rendering library used by xmobar
        bat
      ])
      ++ (with pkgs.nur; [
        # nextcloud-client wrapper that waits for KeePass Secret Service Integration
        repos.mrcpkgs.nextcloud-no-de
      ]);
  };
}
