{ config, pkgs, ... }:
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

  # Packages we want system-wide. Git is essential to obtain this repo before
  # installing home-manager. The others are optional.
  environment.systemPackages = with pkgs;
    [
      git
      pciutils # for lspci
      sof-firmware # some audio devices need this
      tmux
      vim
      wget
      hdparm
      lm_sensors
    ];

  # Set your time zone.
  time.timeZone = "Asia/Singapore";

  # Disables GUI askpass prompt
  programs.ssh = {
    askPassword = "";
    # disable checking for home network
    extraConfig = ''
      Host *.home.com
        StrictHostKeyChecking no

      Host *
        HostKeyAlgorithms +ssh-rsa
        PubkeyAcceptedKeyTypes +ssh-rsa
    '';
  };

  powerManagement.cpuFreqGovernor = "schedutil";

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
    DefaultTimeoutStopSec=30s
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
}
