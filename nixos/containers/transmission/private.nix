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
    ];
    ports = [ "${toString port}:9091" ];
    extraOptions = [ "--cap-add=NET_ADMIN" ];
    environment =
    {
      GLOBAL_APPLY_PERMISSIONS = "false";
      LOCAL_NETWORK= "192.168.1.0/24";
      NORDVPN_CATEGORY = "standard";
      NORDVPN_COUNTRY = "CH";
      NORDVPN_PROTOCOL = "UDP";
      OPENVPN_PASSWORD = "**None**";
      OPENVPN_PROVIDER = "NORDVPN";
      OPENVPN_USERNAME = "**None**";
      PGID = builtins.toString config.users.groups."users".gid;
      PUID = builtins.toString config.users.users."pengu".uid;

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
      TRANSMISSION_RPC_HOST_WHITELIST = "*.home.com";
      TRANSMISSION_RPC_PASSWORD = "pengu";
      TRANSMISSION_RPC_USERNAME = "pengu";
      TRANSMISSION_RPC_WHITELIST = "192.168.1.*,127.0.0.1";
      TRANSMISSION_WEB_UI = "flood-for-transmission";
    };
  };

  sops.secrets."openvpn-credentials" = {};
}
