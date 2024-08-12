{ config, pkgs, lib, ... }:

{
  imports = [
    ./common/boot.nix
    ./common/common.nix
    ../misc/nvidia.nix
    ../misc/xserver.nix
    ../misc/promicro.nix
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

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/06fa2c05-663a-4b3b-9f3f-7d1627190338";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/1618-9A73";
      fsType = "vfat";
    };

    "/network/public" = {
      device = "//home.lan/public";
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
      device = "//home.lan/private";
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
