{ config, pkgs, libs, ... }:

{
  boot.loader = {
    timeout = 30;
    grub = {
      enable = true;
      version = 2;
      gfxmodeEfi = "640x480";
      gfxmodeBios = "640x480";
      useOSProber = true;
      configurationLimit = 5;
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
    hdparm              # for spin down later on
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

  # Add a user that can sudo.
  users.users.pengu = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  virtualisation.virtualbox.host.enable = true;
  users.extraGroups.vboxusers.members = [ "pengu" ];

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  # Installs fcitx for CJK input. Remove if not needed.
  i18n.inputMethod.enabled = "fcitx";
  i18n.inputMethod.fcitx.engines = with pkgs.fcitx-engines; [ libpinyin ];

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

  system.stateVersion = "20.09";
}
