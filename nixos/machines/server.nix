args@{ config, pkgs, ... }:
let
  giteaPort = 3001;
  giteaSshPort = 3002;

  testPort = 7860;
  test2Port = 7861;

  # hydraPort = 4000;
  jellyfinPort = 5000;
  stashPort = 5001;
  mediawikiPort = 7000;
  # nixservePort = 7100;
  syncthingPort = 7300;

  transmissionPort = 10000;
  transmissionPrivPort = 10001;

  postgresPort = 5432;

  prometheusPort = 6000;
  nodeExporterPort = 6001;
  zfsExporterPort = 6002;
  nginxExporterPort = 6003;
  nginxLogExporterPort = 6004;
  grafanaPort = 6100;
  lokiPort = 6200;

  minioPort = 6450;
  minioUIPort = 6451;
in
{
  imports =
    [
      ./common/boot.nix
      ./common/common.nix

      # ((import ../containers/elasticsearch) { })
      ((import ../containers/jellyfin) { port = jellyfinPort; })

      ((import ../containers/transmission/private.nix) (args // {
        port = transmissionPrivPort;
      }))
      ((import ../containers/transmission) (args // {
        port = transmissionPort;
      }))
      ((import ../containers/stashapp) (args // {
        port = stashPort;
      }))

      # ((import ../services/acme) args)
      ../services/fail2ban
      ((import ../services/gitea) (args // {
        inherit giteaPort giteaSshPort postgresPort;
      }))

      # ((import ../services/minio) (args // { inherit minioPort minioUIPort; }))
      ((import ../services/postgresql { port = postgresPort; }))

      (import ../services/prometheus {
        inherit prometheusPort nodeExporterPort nginxExporterPort
          nginxLogExporterPort zfsExporterPort;
        otherScrapePorts = [
        ];
      })
      (import ../services/grafana {
        inherit config grafanaPort postgresPort;
      })
      # (import ../services/loki { inherit lokiPort; })
      # (import ../services/promtail { inherit lokiPort; })

      ((import ../services/openssh) args)
      ../services/samba
      ((import ../services/syncthing) (args // {
        port = syncthingPort;
      }))
      ../services/zfs

      ((import ../services/nginx) (args // {
        portMap = [
          # format: [host port openToPublic?]
          [ "test" testPort false ]
          [ "test2" test2Port false ]

          [ "gitea" giteaPort true ]
          [ "jellyfin" jellyfinPort true ]
          [ "stash" stashPort false ]
          [ "sync" syncthingPort false ]
          [ "torrents" transmissionPort false ]
          [ "transmission" transmissionPrivPort false ]
          [ "wiki" mediawikiPort true ]

          [ "grafana" grafanaPort true ]
          [ "loki" lokiPort false ]
          [ "prometheus" prometheusPort false ]

          [ "minio" minioUIPort false ]
          [ "s3" minioPort true ]
        ];
      }))
    ];

  sops = {
    defaultSopsFile = ../secrets/secrets.yaml;
    gnupg.sshKeyPaths = [ ];
    age.keyFile = "/var/lib/sops/age/keys.txt";
  };

  virtualisation.oci-containers.backend = "docker";
  virtualisation.docker = {
    enable = true;
    storageDriver = "overlay2";
  };

  # Use the GRUB 2 boot loader.
  boot = {
    initrd = {
      supportedFilesystems = [ "zfs" ];
    };
    kernelModules = [ "kvm-amd" "amdgpu" ];
    supportedFilesystems = [ "zfs" ];
    zfs = {
      requestEncryptionCredentials = true;
      extraPools = [ "tank" ];
    };
    loader.grub = {
      memtest86.enable = true;
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
    interfaces.enp8s0 = {
      useDHCP = true;
      wakeOnLan.enable = true;
    };

    nat = {
      # Remap container traffic to use external IP address
      enable = true;
      internalInterfaces = [ "ve-+" ];
      externalInterface = "enp8s0";
    };

    firewall = {
      enable = true;
    };
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
      # substituters = [ "daemon?priority=50" ];
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
