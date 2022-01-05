{ config, pkgs, lib, ... }:
let
  giteaPort = 3001;
  giteaSshPort = 3002;
  gollumPort = 4000;
  grafanaPort = 3000;
  jellyfinPort = 8096;
  mealiePort = 9925;
  mediawikiPort = 4001;
  stashPort = 9999;
  syncthingPort = 8384;
  transRpcPort = 9001;
  trans2RpcPort = 9002;
  wireguardPort = 50000;
in
{
  imports =
    [
      <home-manager/nixos>
      <sops-nix/modules/sops>

      ((import ../containers/gitea/default.nix) {
        port = giteaPort; sshPort = giteaSshPort;
      })
      ((import ../containers/stashapp/default.nix) { port = stashPort; })
      ((import ../containers/transmission/default.nix) {
        config = config; port = transRpcPort;})
      ((import ../containers/transmission/private.nix) {
        config = config; lib = lib; port = trans2RpcPort;
      })
      ((import ../containers/jellyfin/default.nix) { port = jellyfinPort; })
      ((import ../containers/mealie/default.nix) { port = mealiePort; })
      ((import ../containers/mediawiki/default.nix) {
        config = config; pkgs = pkgs; port = mediawikiPort;
      })

      ../services/fail2ban/default.nix
      ../services/openssh/default.nix
      ../services/samba/default.nix
      ((import ../services/syncthing/default.nix) {
        config = config; lib = lib; port = syncthingPort;
      })
      ../services/zfs/default.nix

      ((import ../services/nginx/default.nix) {
          config = config;
          portMap = [
            ["gitea" giteaPort]
            ["ssh.gitea" giteaSshPort]
            ["jellyfin" jellyfinPort]
            ["mealie" mealiePort]
            ["stash" stashPort]
            ["sync" syncthingPort]
            ["transmission" transRpcPort]
            ["torrent" trans2RpcPort]
            ["wiki" mediawikiPort]
          ];
      })
    ];

  sops = {
    defaultSopsFile = ../secrets/secrets.yaml;
    gnupg.sshKeyPaths = [];
    age.keyFile = "/var/lib/sops/age/keys.txt";
  };

  # Use the GRUB 2 boot loader.
  boot.supportedFilesystems = [ "zfs" ];
  boot.zfs = {
    requestEncryptionCredentials = true;
    extraPools = [ "tank" ];
  };
  boot.loader.grub = {
    efiInstallAsRemovable = true;
    efiSupport = true;
    device = "nodev";
  };

  networking = {
    hostName = "obsidian";
    # Random 8 digit hex string for ZFS
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
      allowedUDPPorts = [ ];
    };
  };

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  users.users.pengu = {
    extraGroups = [ config.users.groups.keys.name ];
  };

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    users.pengu = import ../home-manager/server.nix;
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
    tcpdump
    wget
  ];

  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # 250 is 5 hours. Visit man hdparm for details.
  powerManagement.powerUpCommands = ''
    ${pkgs.hdparm}/sbin/hdparm -S 250 /dev/sda
    ${pkgs.hdparm}/sbin/hdparm -S 250 /dev/sdb
    ${pkgs.hdparm}/sbin/hdparm -S 250 /dev/sdd
    ${pkgs.hdparm}/sbin/hdparm -S 250 /dev/sde
    ${pkgs.hdparm}/sbin/hdparm -S 250 /dev/sdf
    ${pkgs.hdparm}/sbin/hdparm -S 250 /dev/sdg
  '';

  time.timeZone = "Asia/Singapore";

  system.stateVersion = "20.09";
}
