args@{ config, pkgs, lib, ... }:
let
  giteaPort = 3001;
  giteaSshPort = 3002;

  devboxPort = 4001;
  # hydraPort = 4000;
  jellyfinPort = 5000;
  mediawikiPort = 7000;
  nixservePort = 7100;
  stashPort = 7200;
  syncthingPort = 7300;
  wireguardPort = 7400;

  transmissionPort = 10000;
  transmissionPrivPort = 10001;

  postgresPort = 5432;

  prometheusPort = 6000;
  nodeExporterPort = 6001;
  zfsExporterPort = 6002;
  nginxExporterPort = 6003;
  nginxLogExporterPort = 6004;
  dcgmExporterPort = 6005;
  grafanaPort = 6100;
  lokiPort = 6200;
in
{
  imports =
    [
      ./common.nix
      ../misc/nvidia.nix

      ((import ../containers/devbox) ({ inherit devboxPort; }))
      ((import ../containers/dcgm-exporter) ({ inherit dcgmExporterPort; }))
      # ((import ../containers/elasticsearch) { })
      ((import ../containers/jellyfin) { port = jellyfinPort; })
      # ((import ../containers/mediawiki) (args // {
      #   port = mediawikiPort;
      # }))
      ((import ../containers/stashapp) (args // {
        port = stashPort;
      }))
      ((import ../containers/transmission/private.nix) (args // {
        port = transmissionPrivPort;
      }))
      ((import ../containers/transmission) (args // {
        port = transmissionPort;
      }))

      # ((import ../misc/wireguard) (args // {
      #   wireguardPort = wireguardPort;
      # }))

      ((import ../services/acme) args)
      ../services/fail2ban
      ((import ../services/gitea) (args // {
        inherit giteaPort giteaSshPort postgresPort;
      }))
      # (import ../services/hydra {
      #   port = hydraPort;
      #   dbPort = postgresPort;
      # })
      ((import ../services/nix-serve (args // { port = nixservePort; })))
      ((import ../services/postgresql { port = postgresPort; }))

      (import ../services/prometheus {
        inherit prometheusPort nodeExporterPort nginxExporterPort
          nginxLogExporterPort zfsExporterPort;
        otherScrapePorts = [
          dcgmExporterPort
        ];
      })
      (import ../services/grafana {
        inherit config grafanaPort postgresPort;
      })
      (import ../services/loki { inherit lokiPort; })
      (import ../services/promtail { inherit lokiPort; })

      ((import ../services/openssh) args)
      ../services/samba
      ((import ../services/syncthing) (args // {
        port = syncthingPort;
      }))
      ../services/zfs

      ((import ../services/nginx) (args // {
        portMap = [
          # format: [host port openToPublic?]
          [ "error" 65500 true ]
          [ "test" 7860 false ]
          [ "play" 7861 true ]

          [ "devbox" devboxPort true ]

          [ "gitea" giteaPort true ]
          [ "grafana" grafanaPort true ]
          # [ "hydra" hydraPort true ]
          [ "jellyfin" jellyfinPort true ]
          [ "loki" lokiPort true ]
          [ "nix-cache" nixservePort true ]
          [ "prometheus" prometheusPort false ]
          [ "stash" stashPort true ]
          [ "sync" syncthingPort false ]
          [ "torrents" transmissionPort false ]
          [ "transmission" transmissionPrivPort false ]
          [ "wiki" mediawikiPort true ]
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
    initrd = {
      supportedFilesystems = [ "zfs" ];
    };
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
      ipv4.routes = [
        {
          address = "192.168.10.1";
          prefixLength = 32;
        }
      ];
    };

    # Local
    interfaces.enp35s0 = {
      useDHCP = true;
      ipv4.routes = [
        {
          address = "192.168.10.0";
          prefixLength = 24;
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
        80
        443
      ];
      allowedUDPPorts = [
      ];
    };
  };

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  home-manager = {
    users.pengu = import ../home-manager/server.nix;
  };

  users.users = {
    pengu = {
      extraGroups = [ config.users.groups.keys.name ];
    };
    # This is a public user made available to NFS and Samba
    fileshare = {
      isSystemUser = true;
      group = "users";
    };
  };

  users.groups = {
    users = { gid = 100; };
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
        "*"
      ];
      trusted-users = [
        "@hydra"
        "@nixbld"
        "@wheel"
        "@nix-serve"
        "root"
      ];
      # sandbox = "relaxed";
      substituters = [ "daemon?priority=50" ];
    };
  };

  services.xserver.enable = false;

  services.cron = {
    enable = false;
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

  zramSwap = {
    enable = true;
    algorithm = "zstd";
  };

  system.stateVersion = "22.11";
}
