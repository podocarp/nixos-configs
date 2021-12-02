# This is a minimal config to get things started.

{ config, pkgs, lib, ... }:
{
  imports =
    [
      ./hardware-configuration.nix
      ../services/openssh/default.nix
    ];

  boot.initrd.supportedFilesystems = [ "zfs" ];
  boot.supportedFilesystems = [ "zfs" ];
  boot.zfs = {
    requestEncryptionCredentials = true;
  };
  boot.loader.grub = {
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

  system.stateVersion = "20.09";
}
