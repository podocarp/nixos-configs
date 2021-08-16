{ config, pkgs, libs, ... }:

{
  boot.loader.grub.device = "nodev";
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.efiInstallAsRemovable = true;
  boot.extraModprobeConfig = ''
  '';

  networking.hostName = "omen"; # Define your hostname.

  # nixpkgs.config.packageOverrides = pkgs: {
  # };

  services.xserver.videoDrivers = [ "nvidia" ];

  # Some hardware acceleration things.
  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  fonts.fontconfig.subpixel.rgba = "none";
}
