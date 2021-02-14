{ config, pkgs, libs, ... }:

{
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.useOSProber = true;

  boot.kernel.sysctl = {
    "kernel.nmi_watchdog" = 0;
    "vm.dirty_writeback_centisecs" = 6000;
    "vm.laptop_mode" = 5;
  };

  # Packages we want system-wide. Git is essential to obtain this repo before
  # installing home-manager. The others are optional.
  environment.systemPackages = with pkgs; [
    git
    pciutils            # for lspci
    vim
    wget
    sof-firmware
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

  # This enables the changing of gtk themes by home-manager.
  programs.dconf.enable = true;
  services.dbus.packages = with pkgs; [ gnome3.dconf ];

  # Disables GUI askpass prompt
  programs.ssh.askPassword = "";

  services.xserver = {
    enable = true;
    xkbOptions = "caps:escape"; # this maps caps to escape.

    libinput = {
      enable = true;
      touchpad.scrollButton = 2;
    };

    # This starts ~/.xsession, which allows home-manager to control some things.
    displayManager.session = [
      {
        name = "xsession";
        start = "${pkgs.runtimeShell} $HOME/.xsession & waitPID=$!";
        manage = "window";
      }
    ];

    useGlamor = true;
  };

  # First line adds something for trackpoints. Doesn't matter if you lack one.
  # Second enables link power managerment.
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="input", ATTR{name}=="TPPS/2 IBM TrackPoint", ATTR{device/press_to_select}="1"
    ACTION=="add", SUBSYSTEM=="scsi_host", KERNEL=="host*", ATTR{link_power_management_policy}="med_power_with_dipm"
  '';

  # Add a user that can sudo.
  users.users.pengu = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  fonts.fontconfig.enable = true;
  fonts.fontconfig.subpixel.rgba = "none";

  # Installs fcitx for CJK input. Remove if not needed.
  i18n.inputMethod.enabled = "fcitx";
  i18n.inputMethod.fcitx.engines = with pkgs.fcitx-engines; [ libpinyin ];

  # Kills hanging services faster.
  systemd.extraConfig = ''
    DefaultTimeoutStopSec=30s
  '';

  nixpkgs.config.allowUnfree = true;

  system.stateVersion = "20.09";
}
