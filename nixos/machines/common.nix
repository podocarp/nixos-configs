{ config, pkgs, libs, ... }:

{
  boot.loader = {
    timeout = 5;
    grub = {
      enable = true;
      version = 2;
      gfxmodeEfi = "640x480";
      gfxmodeBios = "640x480";
      configurationLimit = 3;
    };
  };

  boot.kernel.sysctl = {
    "kernel.nmi_watchdog" = 0;
    "vm.dirty_writeback_centisecs" = 6000;
  };

  boot.crashDump.enable = true;

  # Packages we want system-wide. Git is essential to obtain this repo before
  # installing home-manager. The others are optional.
  environment.systemPackages = with pkgs; [
    git
    pciutils            # for lspci
    sof-firmware        # some audio devices need this
    tmux
    vim
    wget
    hdparm
    lm_sensors
  ];

  # Set your time zone.
  time.timeZone = "Asia/Singapore";

  # Add your wireless networks.
  networking = {
    # The global useDHCP flag is deprecated. Per-interface useDHCP will be
    # mandatory in the future. However to supprort multiple machines, we will
    # still continue using it as long as it works.
    useDHCP = true;
  };

  # Disables GUI askpass prompt
  programs.ssh = {
    askPassword = "";
    # disable checking for home network
    extraConfig = ''
      Host *.home.com
        StrictHostKeyChecking no
    '';
  };

  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="input", ATTR{name}=="TPPS/2 IBM TrackPoint", ATTR{device/press_to_select}="1"
  '';

  services.tlp = {
    enable = true;
    settings = {
        "TLP_DEFAULT_MODE" = "AC";
        "TLP_PERSISTENT_DEFAULT" = 1;
    };
  };

  powerManagement.cpuFreqGovernor = "schedutil";

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

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };

  nix.optimise = {
    automatic = true;
    dates = [ "weekly" ];
  };

  nix.allowedUsers = [ "@wheel" ];
}