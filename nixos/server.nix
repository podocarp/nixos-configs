{ config, pkgs, ... }:

let
  jxdkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCo9zWNi53WN8NRWNm2ZwMAVy3YPK7IS9nbKo0hHhy+HYjwwuNx0PJg1XaUuJpbN1nKiHh2UJCRO/OsZNFtLz23abMd41jjiNT5+u2NWYjZYC2uZnqirJXr2VbJDHKWndyrC3EZhDdx6MZ44zDC9LirTZETgHgc75I24HvLLlkSfSVjOlMUe1SP38+gpypruzIEA9olLoQ81UjxWarr1w7E5BWKfzvjuzNVKzf3Yl4t6hxpvvHU4Gg8Yuu7fyf0dmNpC6r+HC4qGNS/3MkZwFiExg+k2ACXS0yBPA+40ANQYsPiEGhTLvpusK4BvstV7AnbRLFdrGLTs6E+2XZCaAK5 openpgp:0x79E90D11";
  # This path is used by Let's encrypt and all services that use it to generate
  # and withdraw certs
  webroot = "/var/www/hs";
  trans_rpc_port = 9091;
  jellyfin_port = 8096;
  servicesToPortMapping = [
    ["transmission" (toString trans_rpc_port)]
    ["jellyfin" (toString jellyfin_port)]
  ];
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix

      # ./containers/tinode/default.nix
      ((import ./containers/prosody/default.nix) {pkgs = pkgs; dir = webroot;})
      ((import ./containers/transmission/default.nix) {config = config; port = trans_rpc_port;})
      ./containers/jellyfin/default.nix

      ((import ./services/acme/default.nix) {dir = webroot;})
      ./services/samba/default.nix
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
    hostName = "obsidian";
    # Random 8 digit hex string for ZFS to work
    hostId = "492A28F4";

    # Internet facing
    interfaces.enp36s0 = {
      useDHCP = true;
    };

    # Local
    interfaces.enp35s0 = {
      useDHCP = false;
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
          via = "192.168.1.1";
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

  users.users.pengu = {
    isNormalUser = true;
    openssh.authorizedKeys.keys = [ jxdkey ];
    uid = 1000;
  };

  users.users.git = {
    isNormalUser = true;
    home = "/tank/git/";
    openssh.authorizedKeys.keys = [ jxdkey ];
  };

  # This is a public user made available to NFS and Samba
  users.users.fileshare = {
    isSystemUser = true;
    # createHome = false;
    # shell = "/run/current-system/sw/bin/nologin";
    # uid = 42069;
    group = "users";
  };

  users.groups."users".gid = 100;
  # users.groups."fileshare".gid = 42069;

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

    # virtualHosts."obsidian" = {
    #   locations."/" = {
    #     alias = (builtins.toFile "index.html"
    #     ("<html><h1>Welcome. A list of services:</h1><ul>" +
    #     (builtins.foldl'
    #     (x: y:
    #       x + ''<li><a href="http://obsidian:${toString (builtins.elemAt y 1)}">'' +
    #       ''${builtins.elemAt y 0}</a></li>'') "" servicesToPortMapping
    #     ) + "</ul></html>"));
    #   };

    #   listen = [{
    #     ip = "*";
    #     port = 1234;
    #   }];
    # };

    # virtualHosts."home" = {
    #   serverAliases = ["home"];
    #   extraConfig = ''
    #     ProxyRequests Off
    #     ProxyPreserveHost On

    #     ProxyPass / http://localhost:1234/
    #     ProxyPassReverse / http://localhost:1234/
    #   '';
    # };

    virtualHosts = builtins.listToAttrs (map
      (xs: {
        name = builtins.elemAt xs 0;
        value = {
          serverAliases = ["${builtins.elemAt xs 0}.home.com"];
          extraConfig = ''
            ProxyRequests Off
            ProxyPreserveHost On

            ProxyPass / http://localhost:${builtins.elemAt xs 1}/
            ProxyPassReverse / http://localhost:${builtins.elemAt xs 1}/
          '';
        };
      }) servicesToPortMapping
    );
  };

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
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

  # 242 is 1 hour. Visit man hdparm
  powerManagement.powerUpCommands = ''
    ${pkgs.hdparm}/sbin/hdparm -S 242 /dev/sda
    ${pkgs.hdparm}/sbin/hdparm -S 242 /dev/sdb
    ${pkgs.hdparm}/sbin/hdparm -S 242 /dev/sdd
    ${pkgs.hdparm}/sbin/hdparm -S 242 /dev/sde
    ${pkgs.hdparm}/sbin/hdparm -S 242 /dev/sdf
    ${pkgs.hdparm}/sbin/hdparm -S 242 /dev/sdg
  '';
}
