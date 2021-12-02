{ config, port, ... }:
let
  dir = "/var/lib/transmission";
in
{
  containers.transmission = {
    autoStart = true;
    ephemeral = true;
    bindMounts = {
      "${dir}" = {
        hostPath = "/tank/local/transmission";
        isReadOnly = false;
      };
      "/Downloads" = {
        hostPath = "/tank/public/Downloads";
        isReadOnly = false;
      };
    };

    config = {
      users.users."pengu" = {
        isSystemUser = true;
        uid = config.users.users.pengu.uid;
        group = "users";
      };

      users.groups."users".gid = config.users.groups."users".gid;

      services.transmission = {
        enable = true;
        home = dir;
        user = "pengu";
        group = "users";
        settings = {
          download-dir = "/Downloads";
          incomplete-dir-enabled = false;

          rpc-bind-address = "0.0.0.0";
          rpc-port = port;
          rpc-host-whitelist = "*.home.com";
          rpc-whitelist = "192.168.1.*,127.0.0.1";

          peer-port-random-on-start = true;

          download-queue-enabled = true;
          download-queue-size = 20;
          queue-stalled-enabled = true;
          queue-stalled-minutes = 10;
          seed-queue-enabled = true;
          seed-queue-size = 5;

          idle-seeding-limit-enabled = true;
          idle-seeding-limit = 30;
          ratio-limit-enabled = true;
          ratio-limit = 1;
        };
      };
      networking.firewall.enable = false;
    };
  };
}
