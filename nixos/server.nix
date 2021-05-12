{ config, pkgs, ... }:

let
  jxdkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCo9zWNi53WN8NRWNm2ZwMAVy3YPK7IS9nbKo0hHhy+HYjwwuNx0PJg1XaUuJpbN1nKiHh2UJCRO/OsZNFtLz23abMd41jjiNT5+u2NWYjZYC2uZnqirJXr2VbJDHKWndyrC3EZhDdx6MZ44zDC9LirTZETgHgc75I24HvLLlkSfSVjOlMUe1SP38+gpypruzIEA9olLoQ81UjxWarr1w7E5BWKfzvjuzNVKzf3Yl4t6hxpvvHU4Gg8Yuu7fyf0dmNpC6r+HC4qGNS/3MkZwFiExg+k2ACXS0yBPA+40ANQYsPiEGhTLvpusK4BvstV7AnbRLFdrGLTs6E+2XZCaAK5 openpgp:0x79E90D11";
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./containers/tinode/default.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.supportedFilesystems = [ "zfs" ];
  boot.zfs.requestEncryptionCredentials = true;
  boot.zfs.extraPools = [ "tank" ];
  boot.zfs.forceImportAll = false;
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.device = "nodev";
  boot.loader.efi.canTouchEfiVariables = true;

  time.timeZone = "Asia/Singapore";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future.
  # networking.useDHCP = false;
  networking = {
    # Define your hostname.
    hostName = "obsidian";
    # Random 8 digit hex string for ZFS to work
    hostId = "492A28F4";

    # Internet facing
    interfaces.enp36s0 = {
      useDHCP = true;
      ipv4.addresses = [
        {
          address = "192.168.1.108";
          prefixLength = 24;
        }
      ];
      ipv4.routes = [
        {
          address = "0.0.0.0";
          prefixLength = 0;
          via = "192.168.0.1";
          options = {
            dev = "enp36s0";
          };
        }
      ];
    };

    # Local
    interfaces.enp35s0 = {
      ipv4.addresses = [
        {
          address = "192.168.1.107";
          prefixLength = 24;
        }
      ];
      ipv4.routes = [
        {
          address = "192.168.1.0";
          prefixLength = 24;
          options = {
            dev = "enp35s0";
          };
        }
      ];
    };

    firewall.enable = false;
  };
  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  users.users.pengu.openssh.authorizedKeys.keys = [ jxdkey ];

  users.users.git = {
    isNormalUser = true;
    home = "/tank/git/";
    openssh.authorizedKeys.keys = [ jxdkey ];
  };

  # This is a public user made available to NFS and Samba
  users.users.fileshare = {
    isNormalUser = true;
    createHome = false;
    shell = "/run/current-system/sw/bin/nologin";
    uid = 42069;
    group = "fileshare";
  };

  users.groups = {
    fileshare.gid = 42069;
  };

  environment.systemPackages = with pkgs; [
    bonnie
    fio
    git
    hdparm
    iftop
    iotop
    iperf
    neovim
    pciutils # for lspci
    smbclient
    tmux
    wget
  ];

  # Enable the Apache server
  services.httpd = {
    enable = true;
    adminAddr = "xdjiaxd@gmail.com";
    extraModules = [
      "proxy"
      "proxy_http"
      "proxy_wstunnel"
    ];
  };

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };

  services.samba = {
    enable = true;
    securityType = "user";
    shares.public = {
      path = "/tank/public";
      writeable = "yes";
      browseable = "yes";
    };
    shares.private= {
      path = "/tank/private";
      writeable = "yes";
      browseable = "yes";
      public = "no";
      "valid users" = "pengu";
    };
    shares.global = {
      "usershare path" = "/var/lib/samba/usershares";
      "usershare max shares" = "100";
      "usershare allow guests" = "yes";
      "usershare owner only" = "no";
      "server min protocol" = "SMB2_02";
    };
  };

  services.zfs = {
    autoSnapshot = {
      enable = true;
      frequent = 0;
      hourly = 0;
      daily = 7;
      weekly = 4;
      monthly = 0;
    };
    autoScrub = {
      enable = true;
      interval = "monthly";
    };
  };

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };

  nix.optimise = {
    automatic = true;
    dates = [ "weekly" ];
  };
}
