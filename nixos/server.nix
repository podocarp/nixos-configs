{ config, pkgs, lib, ... }:

let
  jxdkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCo9zWNi53WN8NRWNm2ZwMAVy3YPK7IS9nbKo0hHhy+HYjwwuNx0PJg1XaUuJpbN1nKiHh2UJCRO/OsZNFtLz23abMd41jjiNT5+u2NWYjZYC2uZnqirJXr2VbJDHKWndyrC3EZhDdx6MZ44zDC9LirTZETgHgc75I24HvLLlkSfSVjOlMUe1SP38+gpypruzIEA9olLoQ81UjxWarr1w7E5BWKfzvjuzNVKzf3Yl4t6hxpvvHU4Gg8Yuu7fyf0dmNpC6r+HC4qGNS/3MkZwFiExg+k2ACXS0yBPA+40ANQYsPiEGhTLvpusK4BvstV7AnbRLFdrGLTs6E+2XZCaAK5 openpgp:0x79E90D11";
  # This path is used by Let's encrypt and all services that use it to generate
  # and withdraw certs
  webroot = "/var/www/hs";
  giteaPort = 3001;
  giteaSshPort = 3002;
  gollumPort = 4000;
  jellyfinPort = 8096;
  syncthingPort = 8384;
  transRpcPort = 9001;
  servicesToPortMapping = [
    ["gitea" (toString giteaPort)]
    ["jellyfin" (toString jellyfinPort)]
    ["sync" (toString syncthingPort)]
    ["transmission" (toString transRpcPort)]
    ["wiki" (toString gollumPort)]
  ];
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix

      ((import ./containers/gitea/default.nix) { port = giteaPort;
        sshPort = giteaSshPort; })
      ((import ./containers/gollum/default.nix) { port = gollumPort; })
      ((import ./containers/syncthing/default.nix) {config = config; lib = lib;
        guiPort = syncthingPort;})
      # ((import ./containers/prosody/default.nix) {pkgs = pkgs; dir = webroot;})
      ((import ./containers/transmission/default.nix) {config = config;
        port = transRpcPort;})
      ./containers/jellyfin/default.nix

      ((import ./services/acme/default.nix) {dir = webroot;})
      ./services/fail2ban/default.nix
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


    nat = {
      # Remap container traffic to use external IP address
      enable = true;
      internalInterfaces = ["ve-+"];
      externalInterface = "enp36s0";
    };

    firewall = {
      enable = true;
      checkReversePath = "loose";
      allowedTCPPorts = [
        69 80 443 giteaSshPort
      ];
    };
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
    fio
    git
    hdparm
    iftop
    iotop
    iperf
    neovim
    openssl
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
      # "proxy_wstunnel" only for websocket tunneling used by tinode
    ];

    virtualHosts = builtins.listToAttrs (map
    (xs:
    let
      name = builtins.elemAt xs 0;
      port = builtins.elemAt xs 1;
    in
      {
        # * is not a valid name
        name = builtins.replaceStrings ["*"] ["fallback"] name;
        value = {
          serverAliases = [
            "${name}.home.com"
          ];
          extraConfig = ''
            ProxyRequests Off
            ProxyPreserveHost On

            ProxyPass / http://localhost:${port}/
            ProxyPassReverse / http://localhost:${port}/
          '';
        };
      }) servicesToPortMapping
    );
  };

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    challengeResponseAuthentication = false;
    ports = [ 69 ];
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

  nix.extraOptions = ''
    plugin-files = ${pkgs.nix-plugins}/lib/nix/plugins
    extra-builtins-file = ${toString ./misc/extra-builtins.nix}
  '';

  # 250 is 5 hours. Visit man hdparm for details.
  powerManagement.powerUpCommands = ''
    ${pkgs.hdparm}/sbin/hdparm -S 250 /dev/sda
    ${pkgs.hdparm}/sbin/hdparm -S 250 /dev/sdb
    ${pkgs.hdparm}/sbin/hdparm -S 250 /dev/sdd
    ${pkgs.hdparm}/sbin/hdparm -S 250 /dev/sde
    ${pkgs.hdparm}/sbin/hdparm -S 250 /dev/sdf
    ${pkgs.hdparm}/sbin/hdparm -S 250 /dev/sdg
  '';

  hardware.fancontrol = {
    enable = false;
    config = ''
    INTERVAL=10
    '';
  };
}
