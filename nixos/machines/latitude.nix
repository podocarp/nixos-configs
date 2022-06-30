{ config, pkgs, libs, ... }:

{
  imports =
    [
      <home-manager/nixos>
      ./common.nix
      ../misc/xserver.nix
    ];

  boot.loader.grub.device = "nodev";
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.efiInstallAsRemovable = true;
  boot.loader.grub.useOSProber = true;

  boot.extraModprobeConfig = ''
    options iwlwifi power_save=1
  '';

  networking.hostName = "work"; # Define your hostname.

  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };

  services.xserver.videoDrivers = [ "modesetting" ];
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

  # WARNING: Machine specific settings. May crash your machine.
  services.undervolt = {
    enable = true;
    coreOffset = -100;
    analogioOffset = -100;
    uncoreOffset = -30;
  };

  services.xserver.dpi = 100;

  system.stateVersion = "22.05";
}
