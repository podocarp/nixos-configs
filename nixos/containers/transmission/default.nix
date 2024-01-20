{ config, port, isPublic ? "true", ... }:
{
  services.transmission = {
    enable = true;
    home = "/tank/local/transmission";
    user = "pengu";
    group = "users";
    settings = {
      download-dir = "/tank/public/Downloads";
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
    credentialsFile = config.sops.secrets.transmission-credentials.path;
  };

  sops.secrets."transmission-credentials" = { };
}
