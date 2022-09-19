{ config, port, ... }:
let
  dataDir = "/var/lib/deluge";
  authFile = "/run/secrets/deluge";
in
{
  containers.deluge = {
    autoStart = true;
    ephemeral = true;
    bindMounts = {
      "${dataDir}" = {
        hostPath = "/tank/local/deluge";
        isReadOnly = false;
      };
      "/downloads" = {
        hostPath = "/tank/public/Downloads";
        isReadOnly = false;
      };
      "${authFile}" = {
        hostPath = config.sops.secrets."deluge".path;
        isReadOnly = true;
      };
    };

    config = {
      users.users."pengu" = {
        isSystemUser = true;
        uid = config.users.users.pengu.uid;
        group = "users";
      };

      users.groups."users".gid = config.users.groups."users".gid;

      services.deluge = {
        enable = true;
        declarative = true;
        user = "pengu";
        group = "users";
        web = {
          enable = true;
          port = port;
        };
        dataDir = dataDir;
        config = {
          "allow_remote" = true;
          "dont_count_slow_torrents" = true;
          "download_location" = "/downloads";
          "max_active_downloading" = 10;
          "max_active_limit" = 20;
          "max_active_seeding" = 3;
          "max_upload_speed" = 100;
          "stop_seed_at_ratio" = true;
          "stop_seed_ratio" = 1;
          "announce_to_all_trackers" = true;
          "announce_to_all_tiers" = true;
        };
        authFile = "/tmp/authfile";
      };

      systemd.services."systemd-tmpfiles-setup".before = [
        "deluged.service"
        "delugeweb.service"
      ];

      # copy the file and set the proper permissions
      systemd.tmpfiles.rules = [
        "C /tmp/authfile - - - - ${authFile}"
        "z /tmp/authfile 0444 pengu users - -"
      ];

      networking.firewall.enable = false;
      system.stateVersion = "22.11";
    };
  };

  sops.secrets."deluge" = { mode = "0440"; };
}
