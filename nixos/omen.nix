{ pkgs, libs, ... }:

{
  boot.loader.grub.device = "nodev";
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.efiInstallAsRemovable = true;
  boot.extraModprobeConfig = ''
  '';

  # boot.kernelParams = [ "acpi_enforce_resources=lax" ];

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

  virtualisation.virtualbox.host.enable = true;
  virtualisation.virtualbox.host.enableExtensionPack = true;
  users.extraGroups.vboxusers.members = [ "pengu" ];

  sound.enable = true;
  hardware.pulseaudio.enable = true;
}
