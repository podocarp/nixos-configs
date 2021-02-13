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

  networking.hostName = "pebble"; # Define your hostname.

  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };

  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia.prime = {
    sync.enable = true;
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

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # WARNING: Machine specific settings. May crash your machine.
  services.undervolt = {
    enable = true;
    coreOffset = -150;
    analogioOffset = -100;
    uncoreOffset = -30;
  };

  services.thinkfan.enable = true;
  services.thinkfan.levels = ''
    (0,     0,      55)
    (1,     53,     65)
    (2,     58,     70)
    (3,     68,     75)
    (6,     73,     80)
    (7,     78,     85)
    (127,   80,     32767)
  '';
}
