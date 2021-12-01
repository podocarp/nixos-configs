{ config, pkgs, lib, ... }:

let
  jxdkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCo9zWNi53WN8NRWNm2ZwMAVy3YPK7IS9nbKo0hHhy+HYjwwuNx0PJg1XaUuJpbN1nKiHh2UJCRO/OsZNFtLz23abMd41jjiNT5+u2NWYjZYC2uZnqirJXr2VbJDHKWndyrC3EZhDdx6MZ44zDC9LirTZETgHgc75I24HvLLlkSfSVjOlMUe1SP38+gpypruzIEA9olLoQ81UjxWarr1w7E5BWKfzvjuzNVKzf3Yl4t6hxpvvHU4Gg8Yuu7fyf0dmNpC6r+HC4qGNS/3MkZwFiExg+k2ACXS0yBPA+40ANQYsPiEGhTLvpusK4BvstV7AnbRLFdrGLTs6E+2XZCaAK5 openpgp:0x79E90D11";
  in
{
  imports =
    [
      ./hardware-configuration.nix
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

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future.
  # networking.useDHCP = false;
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

  users.users.pengu = {
    isNormalUser = true;
    openssh.authorizedKeys.keys = [ jxdkey ];
    uid = 1000;
  };

  users.groups."users".gid = 100;

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    challengeResponseAuthentication = false;
    ports = [ 69 ];
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
