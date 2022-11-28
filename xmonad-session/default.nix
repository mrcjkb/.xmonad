{ pkgs, defaultUser, ...}: 
{
  
  home-manager.users."${defaultUser}" = {
    xdg.configFile."xmonad" = {
      source = ../.;
      recursive = true;
    };
    xdg.configFile."rofi" = {
      source = ../configs/rofi/.;
      recursive = true;
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
        };
        defaultSession = "none+xmonad";
      };
      windowManager = {
        xmonad = {
          enable = true;
          enableContribAndExtras = true;
          extraPackages = hpkgs: [
            hpkgs.xmonad
            hpkgs.xmonad-contrib
            hpkgs.xmonad-extras
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
    # Enable blueman if the DE does not provide a bluetooth management GUI.
    blueman.enable = true;
    pipewire = {
      enable = true;
      alsa.enable = true;
      pulse.enable = true;
    };
  };
  programs = {
    slock.enable = true;
  };
  environment = {
    systemPackages = with pkgs; [
      xmobar-app
      rofi
      ranger # TUI file browser
      alacritty
      dmenu # Expected by xmonad
      gxmessage # Used by xmonad to show help
      xorg.xkill # Kill X windows with the cursor
      pscircle # Generate process tree visualizations
      haskellPackages.greenclip # Clipboard manager for use with rofi
      dunst
      bat
      pavucontrol # PulseAudio volume control UI
      pulseaudio
      brightnessctl # Brightness control CLI
      scrot # A command-line screen capture utility
      #### NUR packages ###
      # (from mrcpkgs NUR package, managed by Marc Jakobi)
      # XXX Note: It may be necessary to update the nur tarball if a package is not found.
      nur.repos.mrcpkgs.nextcloud-no-de # nextcloud-client wrapper that waits for KeePass Secret Service Integration
    ];
  };
}
