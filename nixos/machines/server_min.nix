# This is a minimal config to get things started.

{ config, pkgs, lib, ... }:
{
  imports =
    [
      ../services/openssh/default.nix
    ];

  boot.initrd.supportedFilesystems = [ "zfs" ];
  boot.initrd.kernelModules= [ "amdgpu" ];
  boot.supportedFilesystems = [ "zfs" ];
  boot.zfs = {
    requestEncryptionCredentials = true;
  };
  boot.loader.grub = {
    efiInstallAsRemovable = true;
    efiSupport = true;
    device = "nodev";
  };

  time.timeZone = "Asia/Singapore";

  networking = {
    hostName = "obsidian";
    # Random 8 digit hex string for ZFS to work
    hostId = "492A28F4";
  };

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Add a user that can sudo.
  users.users.pengu = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    uid = 1000;
  };

  environment.systemPackages = with pkgs; [
    git
    hdparm
    iftop
    iotop
    pciutils # for lspci
    tmux
    tcpdump
    wget
  ];

  fileSystems."/" =
    {
      device = "zroot/local/root";
      fsType = "zfs";
    };

  fileSystems."/home" =
    {
      device = "zroot/local/home";
      fsType = "zfs";
    };

  fileSystems."/nix" =
    {
      device = "zroot/nix/nix";
      fsType = "zfs";
    };

  fileSystems."/boot" =
    {
      device = "/dev/nvme0n1p1";
      fsType = "vfat";
    };

  system.stateVersion = "20.09";
}
