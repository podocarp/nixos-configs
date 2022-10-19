{ config, secretsPath, port, homeDir }:
{
  defaultConfig = {
    users.users."pengu" = {
      isSystemUser = true;
      uid = config.users.users.pengu.uid;
      group = "users";
    };

    users.groups."users".gid = config.users.groups."users".gid;

    services.transmission = {
      enable = true;
      home = homeDir;
      user = "pengu";
      group = "users";
      settings = {
        download-dir = "/downloads";
        incomplete-dir-enabled = false;

        rpc-authentication-required = true;
        rpc-bind-address = "0.0.0.0";
        rpc-host-whitelist = "*.home.com,*.jiaxiaodong.com";
        rpc-port = port;
        rpc-whitelist = "192.168.1.*,127.0.0.1";

        peer-port-random-on-start = true;

        download-queue-enabled = true;
        download-queue-size = 20;
        queue-stalled-enabled = true;
        queue-stalled-minutes = 10;
        seed-queue-enabled = true;
        seed-queue-size = 5;

        idle-seeding-limit-enabled = true;
        idle-seeding-limit = 10;
        ratio-limit-enabled = true;
        ratio-limit = 1;
      };
      credentialsFile = secretsPath;
    };

    networking.firewall.enable = false;
    system.stateVersion = "22.11";
  };
}
