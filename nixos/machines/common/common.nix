{ pkgs, ... }:
{
  i18n.defaultLocale = "en_SG.UTF-8";
  i18n.extraLocaleSettings = {
    LANGUAGE = "en_SG.UTF-8";
    LC_ALL = "en_SG.UTF-8";
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Packages we want system-wide.
  environment.systemPackages = with pkgs;
    [
      git
      pciutils # for lspci
      sof-firmware # some audio devices need this
      vim
      wget
      hdparm
      lm_sensors
    ];

  # Set your time zone.
  time.timeZone = "Asia/Singapore";

  # Disables GUI askpass prompt
  programs.ssh = {
    startAgent = true;
    askPassword = "";
    # disable checking for home network
    extraConfig = ''
      Host *.home.lan
        StrictHostKeyChecking no

      Host *
        HostKeyAlgorithms +ssh-rsa
        PubkeyAcceptedKeyTypes +ssh-rsa
    '';
  };

  fonts = {
    packages = with pkgs; [
      liberation_ttf
      corefonts
      (nerdfonts.override {
        fonts = [ "DroidSansMono" ];
      })
    ];

    fontconfig = {
      enable = true;
      antialias = true;

      hinting = {
        enable = true;
        style = "slight";
        autohint = false;
      };

      subpixel = {
        rgba = "rgb";
        lcdfilter = "default";
      };

      defaultFonts = {
        serif = [ "Liberation Serif" "DejaVu Serif" ];
        sansSerif = [ "Liberation Sans" "DejaVu Sans" ];
        monospace = [ "DroidSansM Nerd Font Mono" "Liberation Mono" ];
      };
    };
  };

  documentation = {
    enable = true;
    man.enable = true;
    nixos.enable = true;
    doc.enable = true;
    dev.enable = true;
  };

  # Add a user that can sudo.
  users.users.pengu = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    uid = 1000;
  };

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  security.wrappers = {
    "mount.nfs" = {
      setuid = true;
      setgid = true;
      owner = "root";
      group = "root";
      source = "${pkgs.nfs-utils.out}/bin/mount.nfs";
    };
    "mount.cifs" = {
      setuid = true;
      owner = "root";
      group = "root";
      source = "${pkgs.cifs-utils}/bin/mount.cifs";
    };
  };

  # Kills hanging services faster.
  systemd.extraConfig = ''
    DefaultTimeoutStopSec=60s
  '';

  nixpkgs.config.allowUnfree = true;

  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };

    optimise = {
      automatic = true;
      dates = [ "weekly" ];
    };

    settings = {
      allowed-users = [ "@wheel" ];
      experimental-features = [ "flakes" "nix-command" ];
    };
  };

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  hardware.enableRedistributableFirmware = true;

  services.timesyncd.enable = false;
  services.ntp.enable = false;
  services.chrony.enable = true;
}
