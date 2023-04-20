{ config, pkgs, lib, ... }:

{
  imports =
    [
      ./common.nix
      ../misc/xserver.nix
    ];

  boot.loader.grub = {
    efiSupport = true;
    efiInstallAsRemovable = true;
    device = "nodev";
  };

  boot.supportedFilesystems = [ "ntfs" ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/9a676615-0810-42ef-a749-256eef7bc2c2";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/904C-C2F0";
      fsType = "vfat";
    };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/79ffb9a3-2435-4c31-bd97-6e1f49ef905e"; }];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  sops = {
    defaultSopsFile = ../secrets/work_secrets.yaml;
    gnupg.sshKeyPaths = [ ];
    age.keyFile = "/var/lib/sops/age/keys.txt";
  };

  boot.extraModprobeConfig = ''
    options iwlwifi power_save=1
    options hid_apple fnmode=0
  '';

  networking.hostName = "work"; # Define your hostname.
  networking.networkmanager = {
    enable = true;
  };
  networking.wireless = {
    enable = true;
    userControlled.enable = true;
    environmentFile = config.sops.secrets.work_password_env.path;
    networks = {
      "Sailors-Personal" = {
        authProtocols = [ "WPA-EAP" ];
        auth = ''
          eap=PEAP
          identity="xiaodong.jia"
          password="@PASS@"
        '';
      };
      "potato" = {
        psk = "0015011463";
      };
    };
  };

  networking.openconnect = {
    interfaces = {
      "work" = {
        gateway = "sg.oneconnect.shopeemobile.com/uservpn";
        user = "xiaodong.jia";
        protocol = "anyconnect";
        passwordFile = config.sops.secrets.work_password_vpn.path;
        autoStart = false;
      };
    };
  };

  sops.secrets."work_password_vpn" = { };
  sops.secrets."work_password_env" = { };

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
      vaapiIntel # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  home-manager.users.pengu = import ../home-manager/work_laptop.nix;

  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.bluetooth.enable = true;

  services.xserver.dpi = 100;
  services.xserver.libinput = {
    enable = true;
    touchpad.accelSpeed = "0.3";
  };

  virtualisation.virtualbox.host.enable = true;
  users.extraGroups.vboxusers.members = [ "pengu" ];
  virtualisation.docker.enable = true;
}
