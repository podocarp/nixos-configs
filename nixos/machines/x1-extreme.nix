{ lib, pkgs, ... }:
{
  imports =
    [
      ./common/bluetooth.nix
      ./common/boot.nix
      ./common/common.nix
      ./common/network_drives.nix
      ./common/nvidia.nix
      ./common/wireless.nix
      ./common/xserver.nix
    ];

  boot = {
    loader.grub = {
      efiSupport = true;
      efiInstallAsRemovable = false;
      device = "nodev";
      configurationLimit = lib.mkForce 3;
    };
    loader.efi.canTouchEfiVariables = true;

    initrd.availableKernelModules = [ "battery" ];

    extraModprobeConfig = ''
      options iwlwifi power_save=1
      options thinkpad_acpi fan_control=1
    '';
  };

  sops = {
    defaultSopsFile = ../secrets/secrets.yaml;
    age.keyFile = "/var/lib/sops/age/keys.txt";
  };

  networking.hostName = "x1"; # Define your hostname.
  networking.useDHCP = lib.mkDefault true;

  programs.steam.enable = true;

  services.xserver = {
    videoDrivers = [ "nvidia" ];
    monitorSection = ''
      DisplaySize 344 193
    '';
  };
  services.libinput.touchpad.accelSpeed = "0.3";

  hardware.nvidia.prime = {
    sync.enable = true;
    nvidiaBusId = "PCI:1:0:0";
    intelBusId = "PCI:0:2:0";
  };

  hardware.graphics.extraPackages = [
    pkgs.intel-media-driver
    pkgs.intel-vaapi-driver
  ];
  environment.variables.LIBVA_DRIVER_NAME = lib.mkForce "iHD";

  hardware.cpu.intel.updateMicrocode = true;

  home-manager.users.pengu = import ../home-manager/laptop.nix;

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/06fa2c05-663a-4b3b-9f3f-7d1627190338";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/1618-9A73";
      fsType = "vfat";
      options = [ "fmask=0022" "dmask=0022" ];
    };


  # WARNING: Machine specific settings. May crash your machine.
  services.undervolt = {
    enable = true;
    coreOffset = -100;
    analogioOffset = -100;
    uncoreOffset = -20;
  };

  services.xserver.dpi = 100;

  services.thinkfan.enable = true;
  services.thinkfan.levels = [
    [ 0 0 55 ]
    [ 1 50 58 ]
    [ 2 55 60 ]
    [ 3 60 70 ]
    [ 6 63 73 ]
    [ 7 67 76 ]
    [ 127 70 32767 ]
  ];

  services.throttled.enable = true;

  system.stateVersion = "22.11";
}
