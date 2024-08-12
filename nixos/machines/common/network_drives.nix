{ config, ... }:
{
  fileSystems = {
    "/network/public" = {
      device = "//obsidian-local/public";
      fsType = "cifs";
      options = [
        "_netdev"
        "user"
        "uid=1000"
        "gid=100"
        "forceuid"
        "forcegid"
        "x-systemd.automount"
        "x-systemd.idle-timeout=10min"
        "x-systemd.device-timeout=5s"
        "x-systemd.mount-timeout=5s"
        "cache=loose"
        "credentials=${config.sops.secrets.smb-public-credentials.path}"
      ];
    };

    "/network/private" = {
      device = "//obsidian-local/private";
      fsType = "cifs";
      options = [
        "_netdev"
        "user"
        "uid=1000"
        "gid=100"
        "forceuid"
        "forcegid"
        "x-systemd.automount"
        "x-systemd.mount-timeout=5s"
        "x-systemd.idle-timeout=10min"
        "x-systemd.device-timeout=5s"
        "x-systemd.mount-timeout=5s"
        "cache=loose"
        "credentials=${config.sops.secrets.smb-private-credentials.path}"
      ];
    };
  };

  sops.secrets.smb-public-credentials = { };
  sops.secrets.smb-private-credentials = { };
}
