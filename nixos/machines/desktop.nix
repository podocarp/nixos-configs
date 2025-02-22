{ ... }:
{
  imports = [
    ./common/boot.nix
    ./common/common.nix
    ./common/network_drives.nix
    ./common/nvidia.nix
    ./common/xserver.nix
    ./common/promicro.nix
  ];

  boot = {
    loader.grub = {
      useOSProber = true;
      efiSupport = true;
      efiInstallAsRemovable = true;
      device = "nodev";
    };
    loader.efi.canTouchEfiVariables = false;
    kernelModules = [ "kvm-amd" ];
  };

  services.xserver.dpi = 137;

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

    "/shared" = {
      device = "/dev/disk/by-uuid/D84453A4445383E2";
      fsType = "ntfs";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/1618-9A73";
      fsType = "vfat";
    };
  };
  services.udisks2.enable = true;

  programs.steam = {
    enable = true;
    localNetworkGameTransfers.openFirewall = true;
  };

  networking.hostName = "desktop"; # Define your hostname.

  virtualisation.virtualbox.host.enable = true;
  users.extraGroups.vboxusers.members = [ "pengu" ];

  virtualisation.docker = {
    enable = true;
  };
  virtualisation.oci-containers.backend = "docker";

  home-manager.users.pengu = import ../home-manager/desktop.nix;

  system.stateVersion = "22.11";
}
