{
  pkgs,
  defaultUser,
  ...
}: {
  home-manager.users."${defaultUser}" = {
    xdg.configFile."xmonad" = {
      source = ../../xmonadrc;
      recursive = true;
    };
    xdg.configFile."rofi" = {
      # TODO: use home-manager module
      source = ../../configs/rofi/.;
      recursive = true;
    };
    programs = {
      rofi = {
        package = pkgs.unstable.rofi;
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
        defaultSession = "none+xmonad";
      };
      windowManager = {
        xmonad = {
          enable = true;
          enableContribAndExtras = true;
          extraPackages = hpkgs:
            with hpkgs; [
              xmonad
              xmonad-contrib
              xmonad-extras
            ];
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
        xmobar-app
        greenclip # Clipboard manager for use with rofi
      ])
      ++ (with pkgs.unstable; [
        ranger # TUI file browser
        alacritty
        dmenu # Expected by xmonad
        gxmessage # Used by xmonad to show help
        xorg.xkill # Kill X windows with the cursor
        bat
        pavucontrol # PulseAudio volume control UI
        pulseaudio
        brightnessctl # Brightness control CLI
        scrot # A command-line screen capture utility
        pamixer # PulseAudio volume mixer
        pango # Rendering library used by xmobar
      ])
      ++ (with pkgs.nur; [
        # nextcloud-client wrapper that waits for KeePass Secret Service Integration
        repos.mrcpkgs.nextcloud-no-de
      ]);
  };
}
