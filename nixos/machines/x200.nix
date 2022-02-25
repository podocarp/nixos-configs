{ config, pkgs, libs, ... }:

{
  imports =
    [
      ./common.nix
      ../misc/xserver.nix
      ../hardware-configuration.nix
      ../wireless.nix
      <home-manager/nixos>
    ];

  boot.loader.grub.device = "/dev/sda";

  boot.extraModprobeConfig = ''
    options iwlwifi power_save=1
    options thinkpad_acpi fan_control
  '';

  networking.hostName = "pebble"; # Define your hostname.

  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };

  services.xserver.videoDrivers = [ "intel" ];
  # Some hardware acceleration things.
  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      intel-media-driver # LIBVA_DRIVER_NAME=iHD
      vaapiIntel         # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  home-manager.users.pengu = import ../home-manager/laptop.nix;

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  fonts.fontconfig.subpixel.rgba = "rgb";

  # WARNING: Machine specific settings. May crash your machine.
  services.undervolt = {
    enable = false;
    coreOffset = -150;
    analogioOffset = -100;
    uncoreOffset = -30;
  };

  services.xserver.libinput.enable = true;
  services.xserver.wacom.enable = true;

  services.tlp.enable = true;

  services.thinkfan.enable = true;
  services.thinkfan.levels = [
    [0     0      55]
    [1     50     58]
    [2     55     60]
    [3     60     70]
    [6     63     73]
    [7     67     76]
    [127   70     32767]
  ];
}
