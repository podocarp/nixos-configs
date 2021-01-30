{ config, pkgs, libs, ... }:

{
  imports =
    [
      ''${builtins.fetchGit {
          url = "https://github.com/NixOS/nixos-hardware.git";
	} }/lenovo/thinkpad/x1-extreme''
    ];

  boot.loader.grub.device = "nodev";
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.efiInstallAsRemovable = true;

  boot.extraModprobeConfig = ''
    options iwlwifi power_save=1
    options thinkpad_acpi fan_control
  '';

  boot.blacklistedKernelModules = [ "snd_hda_codec_hdmi" ];

  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };

  services.xserver.videoDrivers = [ "nvidia" "modesetting" ];
  hardware.nvidia.prime = {
    intelBusId = "PCI:0:2:0";
    nvidiaBusId = "PCI:1:0:0";
  };
  # Some hardware acceleration things.
  hardware.opengl = {
    extraPackages = with pkgs; [
      intel-media-driver # LIBVA_DRIVER_NAME=iHD
      vaapiIntel         # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

 # WARNING: Machine specific settings. May crash your machine even if it's the
 # same model (X1 Extreme).
  services.undervolt = {
    enable = true;
    coreOffset = -150;
    analogioOffset = -100;
    uncoreOffset = -30;
  };
}
