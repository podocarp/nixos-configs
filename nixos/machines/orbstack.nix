# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

args@{ modulesPath, config, pkgs, lib, ... }:

{
  imports =
    [
      # Include the default lxd configuration.
      "${modulesPath}/virtualisation/lxc-container.nix"
      ((import ./common/common.nix) args)
    ];

  networking.hostName = "nixos";
  networking.interfaces.eth0.useDHCP = true;

  users.users.bytedance = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  home-manager.users.bytedance = import ../home-manager/orbstack.nix;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

  environment.shellInit = ''
    . /opt/orbstack-guest/etc/profile-early
    # add your customizations here
    . /opt/orbstack-guest/etc/profile-late
  '';

  networking.dhcpcd.extraConfig = ''
    noarp
    noipv6
  '';

  systemd.services."systemd-oomd".serviceConfig.WatchdogSec = 0;
  systemd.services."systemd-resolved".serviceConfig.WatchdogSec = 0;
  systemd.services."systemd-userdbd".serviceConfig.WatchdogSec = 0;
  systemd.services."systemd-udevd".serviceConfig.WatchdogSec = 0;
  systemd.services."systemd-timesyncd".serviceConfig.WatchdogSec = 0;
  systemd.services."systemd-timedated".serviceConfig.WatchdogSec = 0;
  systemd.services."systemd-portabled".serviceConfig.WatchdogSec = 0;
  systemd.services."systemd-nspawn@".serviceConfig.WatchdogSec = 0;
  systemd.services."systemd-networkd".serviceConfig.WatchdogSec = 0;
  systemd.services."systemd-machined".serviceConfig.WatchdogSec = 0;
  systemd.services."systemd-localed".serviceConfig.WatchdogSec = 0;
  systemd.services."systemd-logind".serviceConfig.WatchdogSec = 0;
  systemd.services."systemd-journald@".serviceConfig.WatchdogSec = 0;
  systemd.services."systemd-journald".serviceConfig.WatchdogSec = 0;
  systemd.services."systemd-journal-remote".serviceConfig.WatchdogSec = 0;
  systemd.services."systemd-journal-upload".serviceConfig.WatchdogSec = 0;
  systemd.services."systemd-importd".serviceConfig.WatchdogSec = 0;
  systemd.services."systemd-hostnamed".serviceConfig.WatchdogSec = 0;
  systemd.services."systemd-homed".serviceConfig.WatchdogSec = 0;
  programs.ssh.extraConfig = ''
    Include /opt/orbstack-guest/etc/ssh_config
  '';
  security.pki.certificateFiles = [
    "/opt/orbstack-guest/run/extra-certs.crt"
  ];

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  krb5 = {
    enable = true;
    libdefaults = {
      default_realm = "BYTEDANCE.COM";
      dns_lookup_realm = false;
      dns_lookup_kdc = false;
      kdc_timesync = 1;
      ccache_type = 4;
      forwardable = true;
      proxiable = true;
      ticket_lifetime = "24h";
      renew_time = "7d";
      rdns = false;
      ignore_acceptor_hostname = true;
    };

    realms."BYTEDANCE.COM" = {
      kdc = [
        "krb5auth.byted.org"
        "krb5auth1.byted.org"
        "krb5auth2.byted.org"
        "krb5auth3.byted.org"
        "krb5auth.byted.org"
      ];
      master_kdc = "krb5auth.byted.org";
      admin_server = "krb5auth.byted.org";
      default_domain = "byted.org";
    };

    domain_realm = {
      ".byted.org" = "BYTEDANCE.COM";
      "byted.org" = "BYTEDANCE.COM";
    };
  };
}
