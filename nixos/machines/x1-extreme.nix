{ config, pkgs, libs, ... }:

{
  imports =
    [
      ''${builtins.fetchGit {
          url = "https://github.com/NixOS/nixos-hardware.git";
      }}/lenovo/thinkpad/x1-extreme''
      <home-manager/nixos>
    ];

  boot.loader.grub.device = "nodev";
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.efiInstallAsRemovable = true;

  boot.extraModprobeConfig = ''
    options iwlwifi power_save=1
    options thinkpad_acpi fan_control
  '';

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
    coreOffset = -150;
    analogioOffset = -100;
    uncoreOffset = -30;
  };

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

  system.stateVersion = "20.09";
}
