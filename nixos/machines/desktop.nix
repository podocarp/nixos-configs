{ config, pkgs, lib, ... }:

{
  imports = [
    ./common/boot.nix
    ./common/common.nix
    ../misc/nvidia.nix
    ../misc/xserver.nix
  ];

  boot = {
    loader.grub = {
      useOSProber = true;
      efiSupport = true;
      device = "nodev";
    };
    loader.efi.canTouchEfiVariables = true;
    kernelModules = [ "kvm-amd" ];
  };

  sops = {
    defaultSopsFile = ../secrets/secrets.yaml;
    gnupg.sshKeyPaths = [ ];
    age.keyFile = "/var/lib/sops/age/keys.txt";
  };

  services.udev = {
    enable = true;
    extraRules = ''
      # Atmel DFU
      ### ATmega16U2
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="2fef", TAG+="uaccess"
      ### ATmega32U2
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="2ff0", TAG+="uaccess"
      ### ATmega16U4
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="2ff3", TAG+="uaccess"
      ### ATmega32U4
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="2ff4", TAG+="uaccess"
      ### AT90USB64
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="2ff9", TAG+="uaccess"
      ### AT90USB162
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="2ffa", TAG+="uaccess"
      ### AT90USB128
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="2ffb", TAG+="uaccess"

      # Caterina (Pro Micro)
      ## pid.codes shared PID
      ### Keyboardio Atreus 2 Bootloader
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="1209", ATTRS{idProduct}=="2302", TAG+="uaccess", ENV{ID_MM_DEVICE_IGNORE}="1"
      ## Spark Fun Electronics
      ### Pro Micro 3V3/8MHz
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="1b4f", ATTRS{idProduct}=="9203", TAG+="uaccess", ENV{ID_MM_DEVICE_IGNORE}="1"
      ### Pro Micro 5V/16MHz
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="1b4f", ATTRS{idProduct}=="9205", TAG+="uaccess", ENV{ID_MM_DEVICE_IGNORE}="1"
      ### LilyPad 3V3/8MHz (and some Pro Micro clones)
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="1b4f", ATTRS{idProduct}=="9207", TAG+="uaccess", ENV{ID_MM_DEVICE_IGNORE}="1"
      ## Pololu Electronics
      ### A-Star 32U4
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="1ffb", ATTRS{idProduct}=="0101", TAG+="uaccess", ENV{ID_MM_DEVICE_IGNORE}="1"
      ## Arduino SA
      ### Leonardo
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="2341", ATTRS{idProduct}=="0036", TAG+="uaccess", ENV{ID_MM_DEVICE_IGNORE}="1"
      ### Micro
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="2341", ATTRS{idProduct}=="0037", TAG+="uaccess", ENV{ID_MM_DEVICE_IGNORE}="1"
      ## Adafruit Industries LLC
      ### Feather 32U4
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="239a", ATTRS{idProduct}=="000c", TAG+="uaccess", ENV{ID_MM_DEVICE_IGNORE}="1"
      ### ItsyBitsy 32U4 3V3/8MHz
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="239a", ATTRS{idProduct}=="000d", TAG+="uaccess", ENV{ID_MM_DEVICE_IGNORE}="1"
      ### ItsyBitsy 32U4 5V/16MHz
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="239a", ATTRS{idProduct}=="000e", TAG+="uaccess", ENV{ID_MM_DEVICE_IGNORE}="1"
      ## dog hunter AG
      ### Leonardo
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="2a03", ATTRS{idProduct}=="0036", TAG+="uaccess", ENV{ID_MM_DEVICE_IGNORE}="1"
      ### Micro
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="2a03", ATTRS{idProduct}=="0037", TAG+="uaccess", ENV{ID_MM_DEVICE_IGNORE}="1"

      # hid_listen
      KERNEL=="hidraw*", MODE="0660", GROUP="plugdev", TAG+="uaccess", TAG+="udev-acl"

      # hid bootloaders
      ## QMK HID
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="2067", TAG+="uaccess"
      ## PJRC's HalfKay
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="0478", TAG+="uaccess"

      # APM32 DFU
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="314b", ATTRS{idProduct}=="0106", TAG+="uaccess"

      # GD32V DFU
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="28e9", ATTRS{idProduct}=="0189", TAG+="uaccess"

      # WB32 DFU
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="342d", ATTRS{idProduct}=="dfa0", TAG+="uaccess"
    '';
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/06fa2c05-663a-4b3b-9f3f-7d1627190338";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/1618-9A73";
      fsType = "vfat";
    };

    "/shared" = {
      device = "/dev/disk/by-uuid/D84453A4445383E2";
      fsType = "ntfs";
      options = [ "rw" "uid=${toString config.users.users.pengu.uid}" ];
    };

    "/network/public" = {
      device = "//obsidian-local/public";
      fsType = "cifs";
      options = [
        "_netdev"
        "user"
        "uid=1000"
        "gid=100"
        "forceuid"
        "forcegid"
        "x-systemd.automount"
        "x-systemd.idle-timeout=10min"
        "x-systemd.device-timeout=5s"
        "x-systemd.mount-timeout=5s"
        "cache=loose"
        "credentials=${config.sops.secrets.smb-public-credentials.path}"
      ];
    };

    "/network/private" = {
      device = "//obsidian-local/private";
      fsType = "cifs";
      options = [
        "_netdev"
        "user"
        "uid=1000"
        "gid=100"
        "forceuid"
        "forcegid"
        "x-systemd.automount"
        "x-systemd.mount-timeout=5s"
        "x-systemd.idle-timeout=10min"
        "x-systemd.device-timeout=5s"
        "x-systemd.mount-timeout=5s"
        "cache=loose"
        "credentials=${config.sops.secrets.smb-private-credentials.path}"
      ];
    };
  };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/d9fbd022-c7d0-4ee3-95a1-bbab0e3585f0"; }];

  sops.secrets.smb-public-credentials = { };
  sops.secrets.smb-private-credentials = { };

  powerManagement.cpuFreqGovernor = lib.mkDefault "schedutil";

  networking.hostName = "desktop"; # Define your hostname.

  virtualisation.virtualbox.host.enable = true;
  users.extraGroups.vboxusers.members = [ "pengu" ];

  virtualisation.docker = {
    enable = true;
  };
  virtualisation.oci-containers.backend = "docker";

  home-manager.users.pengu = import ../home-manager/desktop.nix;

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  system.stateVersion = "22.11";
}
