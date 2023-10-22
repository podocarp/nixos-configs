{ config, lib, port, ... }:
let
  dir = "/data";
in
{
  virtualisation.oci-containers.containers."transmission_private" = {
    autoStart = true;
    image = "haugene/transmission-openvpn:latest";
    volumes = [
      "/tank/local/transmission_priv:${dir}"
      "/tank/private/Downloads:/Downloads"
      "/tank/private/Downloaded:/Downloaded"
      "${config.sops.secrets.openvpn-credentials.path}:/config/openvpn-credentials.txt"
      "${config.sops.secrets.openvpn-config.path}:/etc/openvpn/custom/default.ovpn"
    ];
    ports = [ "${toString port}:9091" ];
    extraOptions = [ "--cap-add=NET_ADMIN" ];
    environment =
      {
        GLOBAL_APPLY_PERMISSIONS = "false";
        LOCAL_NETWORK = "192.168.10.0/24";
        OPENVPN_PROVIDER = "custom";
        OPENVPN_PASSWORD = "**None**";
        OPENVPN_USERNAME = "**None**";
        PGID = builtins.toString config.users.groups."users".gid;
        PUID = builtins.toString config.users.users."pengu".uid;
        GITHUB_CONFIG_SOURCE_REPO = "rubbish domain";

        TRANSMISSION_DOWNLOAD_DIR = "/Downloaded";
        TRANSMISSION_DOWNLOAD_QUEUE_ENABLED = "true";
        TRANSMISSION_DOWNLOAD_QUEUE_SIZE = "20";
        TRANSMISSION_HOME = dir;
        TRANSMISSION_INCOMPLETE_DIR = "/Downloads";
        TRANSMISSION_INCOMPLETE_DIR_ENABLED = "true";
        TRANSMISSION_PEER_PORT_RANDOM_ON_START = "true";
        TRANSMISSION_QUEUE_STALLED_ENABLED = "true";
        TRANSMISSION_QUEUE_STALLED_MINUTES = "10";
        TRANSMISSION_RATIO_LIMIT = "1";
        TRANSMISSION_RATIO_LIMIT_ENABLED = "true";
        TRANSMISSION_RPC_AUTHENTICATION_REQUIRED = "true";
        TRANSMISSION_RPC_HOST_WHITELIST = "*.home.lan,*.jiaxiaodong.com";
        TRANSMISSION_RPC_PASSWORD = "pengu";
        TRANSMISSION_RPC_USERNAME = "pengu";
        TRANSMISSION_RPC_WHITELIST = "192.168.10.*,127.0.0.1";
        TRANSMISSION_WEB_UI = "flood-for-transmission";
      };
  };

  sops.secrets."openvpn-credentials" = { };
  sops.secrets."openvpn-config" = { sopsFile = ../../secrets/secrets-certs.yaml; };
  sops.secrets."transmission-credentials" = { };
}
