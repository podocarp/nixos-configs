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
  syncthingPort = 9000;
  transmissionPort = 10000;
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
        port = giteaPort;
        sshPort = giteaSshPort;
      })
      ((import ../containers/jellyfin/default.nix) { port = jellyfinPort; })
      ((import ../containers/mealie/default.nix) { port = mealiePort; })
      # ((import ../containers/mediawiki/default.nix) (args // {
      #   port = mediawikiPort;
      # }))
      ((import ../containers/transmission/default.nix) (args // {
        port = transmissionPort;
      }))

      ((import ../misc/wireguard/default.nix) (args // {
        wireguardPort = wireguardPort;
      }))

      ../services/fail2ban/default.nix
      # ((import ../services/hydra/default.nix {
      #   port = hydraPort;
      #   dbPort = postgresPort;
      # }))
      # ((import ../services/postgresql/default.nix { port = postgresPort; }))
      ../services/openssh/default.nix
      ../services/samba/default.nix
      ((import ../services/syncthing/default.nix) (args // {
        port = syncthingPort;
      }))
      ../services/zfs/default.nix

      ((import ../services/nginx/default.nix) (args // {
        portMap = [
          # format: [host port openToPublic?]
          [ "error" 65500 true ]
          # ["firefly" fireflyPort]
          [ "gitea" giteaPort true ]
          # ["hydra" hydraPort]
          [ "jellyfin" jellyfinPort true ]
          [ "mealie" mealiePort true ]
          [ "sync" syncthingPort true ]
          [ "torrents" transmissionPort true ]
          [ "wiki" mediawikiPort false ]
        ];
      }))
    ];

  sops = {
    defaultSopsFile = ../secrets/secrets.yaml;
    gnupg.sshKeyPaths = [ ];
    age.keyFile = "/var/lib/sops/age/keys.txt";
  };

  virtualisation.oci-containers.backend = "docker";

  # Use the GRUB 2 boot loader.
  boot = {
    initrd.supportedFilesystems = [ "zfs" ];
    initrd.kernelModules = [
      "xhci_pci"
      "ahci"
      "nvme"
      "usb_storage"
      "usbhid"
      "sd_mod"
    ];
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
    kernel.sysctl = {
      "net.core.rmem_max" = 4194304;
      "net.core.wmem_max" = 1048576;
      "net.ipv4.ip_forward" = 1;
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
          prefixLength = 32;
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

    nat = {
      # Remap container traffic to use external IP address
      enable = true;
      internalInterfaces = [ "ve-+" ];
      externalInterface = "enp36s0";
    };

    firewall = {
      enable = true;
      checkReversePath = "loose";
      allowedTCPPorts = [
        69
        80
        443
        giteaSshPort
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

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    users.pengu = import ../home-manager/server.nix;
  };

  # This is a public user made available to NFS and Samba
  users.users = {
    pengu = {
      extraGroups = [ config.users.groups.keys.name ];
    };
    pengu-sftp = {
      isNormalUser = true;
      home = "/tank/public/sftp";
      shell = "/run/current-system/sw/bin/nologin";
      group = "sftp";
    };
    fileshare = {
      isSystemUser = true;
      group = "users";
    };
  };

  users.groups = {
    users = { gid = 100; };
    sftp = { };
  };

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
    settings = {
      allowed-users = [
        "root"
        "@nixbld"
        "@wheel"
        "@hydra"
      ];
      trusted-users = [
        "root"
        "@nixbld"
        "@wheel"
      ];
      sandbox = "relaxed";
    };
  };

  services.cron = {
    enable = true;
    systemCronJobs = [
      "59 22 * * * root date -d '7 hours' +%s > /sys/class/rtc/rtc0/wakealarm"
      "00 23 * * * root poweroff"
    ];
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

  swapDevices =
    [{ device = "/dev/disk/by-uuid/8064cf73-931b-4c5b-8d94-16b4f9272181"; }];

  system.stateVersion = "22.11";
}
