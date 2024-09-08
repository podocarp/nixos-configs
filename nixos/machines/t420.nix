{ config, pkgs, lib, ... }:

{
  imports =
    [
      ./common/boot.nix
      ./common/common.nix
      ./common/network_drives.nix
      ./common/xserver.nix
    ];

  boot = {
    loader.grub.device = "/dev/sda";
    extraModprobeConfig = ''
      options iwlwifi power_save=1
      options snd_hda_intel power_save=1
      options thinkpad_acpi fan_control
    '';
    initrd.availableKernelModules = [ "ehci_pci" "ahci" "usb_storage" "sd_mod" "sdhci_pci" ];
    kernelModules = [ "kvm-intel" ];
    kernelParams = [ "iomem=relaxed" ];
  };


  networking.hostName = "t420"; # Define your hostname.
  networking.useDHCP = lib.mkDefault true;
  networking.wireless = {
    userControlled.enable = true;
    enable = true;
    environmentFile = config.sops.secrets."network_env".path;
    networks = {
      "Wireless@SGx" = {
        auth = ''
          key_mgmt=WPA-EAP
          eap=PEAP
          identity="@WSG_IDENTITY@"
          password="@WSG_PASSWORD@"
        '';
      };
      "potato" = {
        psk = "@POTATO_PSK@";
      };
      "mobile potato" = {
        psk = "@POTATO_PSK@";
      };
    };
  };

  sops = {
    defaultSopsFile = ../secrets/secrets.yaml;
    age.keyFile = "/var/lib/sops/age/keys.txt";
  };

  sops.secrets."network_env" = {
    sopsFile = ../secrets/secrets-network.yaml;
  };

  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="pci" KERNEL=="*" ATTR{power/control}="auto"
    SUBSYSTEM=="" KERNEL=="ata*" ATTR{power/control}="auto"
  '';

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/e1fcfe9c-9bb8-4402-bf1a-345419c8c054";
      fsType = "ext4";
    };

  services.xserver.videoDrivers = [ "intel" ];
  # Some hardware acceleration things.
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = with pkgs; [
      intel-media-driver # LIBVA_DRIVER_NAME=iHD
      vaapiIntel # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  home-manager.users.pengu = import ../home-manager/laptop.nix;

  hardware.pulseaudio.enable = true;
  hardware.enableRedistributableFirmware = true;

  services.xserver.dpi = 90;

  # WARNING: Machine specific settings. May crash your machine.
  services.undervolt = {
    enable = false;
    coreOffset = -150;
    analogioOffset = -100;
    uncoreOffset = -30;
  };

  services.libinput.enable = true;

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

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  system.stateVersion = "24.05"; # Did you read the comment?
}
