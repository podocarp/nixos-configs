args@{ config, pkgs, lib, ... }:
let
  fireflyPort = 2000;
  giteaPort = 3001;
  giteaSshPort = 3002;
  hydraPort = 4000;
  jellyfinPort = 5000;
  mealiePort = 6000;
  mediawikiPort = 7000;
  postgresPort = 7777;
  stashPort = 8000;
  syncthingPort = 9000;
  transRpcPort = 10000;
  trans2RpcPort = 10100;
  wireguardPort = 50000;
in
{
  imports =
    [
      ./common.nix
      <home-manager/nixos>
      <sops-nix/modules/sops>

      # ((import ../containers/firefly/default.nix) (args // {
      #   port = fireflyPort;
      # }))
      ((import ../containers/gitea/default.nix) {
        port = giteaPort; sshPort = giteaSshPort;
      })
      ((import ../containers/jellyfin/default.nix) { port = jellyfinPort; })
      ((import ../containers/mealie/default.nix) { port = mealiePort; })
      ((import ../containers/mediawiki/default.nix) (args // {
        port = mediawikiPort;
      }))
      # ((import ../containers/stashapp/default.nix) { port = stashPort; })
      ((import ../containers/transmission/default.nix) {
        config = config; port = transRpcPort;})
      ((import ../containers/transmission/private.nix) (args // {
        port = trans2RpcPort;
      }))

      ../services/fail2ban/default.nix
      ((import ../services/hydra/default.nix {
        port = hydraPort;
        dbPort = postgresPort;
      }))
      ((import ../services/postgresql/default.nix { port = postgresPort; }))
      ../services/openssh/default.nix
      ../services/samba/default.nix
      ((import ../services/syncthing/default.nix) (args // {
        port = syncthingPort;
      }))
      ../services/zfs/default.nix

      ((import ../services/nginx/default.nix) (args // {
          portMap = [
            ["error" 65535] # acts as a fallback and throws errors
            # ["firefly" fireflyPort]
            ["gitea" giteaPort]
            ["ssh-gitea" giteaSshPort]
            ["hydra" hydraPort]
            ["jellyfin" jellyfinPort]
            ["mealie" mealiePort]
            # ["stash" stashPort]
            ["sync" syncthingPort]
            ["transmission" transRpcPort]
            ["torrent" trans2RpcPort]
            ["wiki" mediawikiPort]
          ];
      }))
    ];

  sops = {
    defaultSopsFile = ../secrets/secrets.yaml;
    gnupg.sshKeyPaths = [];
    age.keyFile = "/var/lib/sops/age/keys.txt";
  };

  virtualisation.oci-containers.backend = "docker";

  # Use the GRUB 2 boot loader.
  boot = {
    initrd.supportedFilesystems = [ "zfs" ];
    initrd.kernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage"
      "usbhid" "sd_mod" ];
    kernelModules = [ "kvm-amd" ];
    supportedFilesystems = [ "zfs" ];
    zfs = {
      requestEncryptionCredentials = true;
      extraPools = [ "tank" ];
    };
    loader.grub = {
      efiInstallAsRemovable = true;
      efiSupport = true;
      device = "nodev";
    };
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
    samba
    tmux
    tcpdump
    wget
  ];

  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    settings = {
      allowed-users = [
        "root" "@nixbld" "@wheel" "@hydra"
      ];
      trusted-users = [
        "root" "@nixbld" "@wheel"
      ];
      sandbox = "relaxed";
    };
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

  fileSystems."/" =
    { device = "zroot/local/root";
      fsType = "zfs";
    };

  fileSystems."/home" =
    { device = "zroot/local/home";
      fsType = "zfs";
    };

  fileSystems."/nix" =
    { device = "zroot/nix/nix";
      fsType = "zfs";
    };

  fileSystems."/boot" =
    { device = "/dev/nvme0n1p1";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/8064cf73-931b-4c5b-8d94-16b4f9272181"; }
    ];

  system.stateVersion = "22.11";
}
