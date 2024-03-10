{ config, port, ... }:
let
  configDir = "/config";
  completeDir = "/Downloaded";
  incompleteDir = "/Downloads";
in
{
  virtualisation.oci-containers.containers."transmission_private" = {
    autoStart = true;
    image = "haugene/transmission-openvpn:5.3.1";
    volumes = [
      "/tank/local/transmission_priv:${configDir}"
      "/tank/private/Downloads:${incompleteDir}"
      "/tank/private/Downloaded:${completeDir}"
    ];
    ports = [ "${toString port}:9091" ];
    extraOptions = [ "--cap-add=NET_ADMIN" ];
    environmentFiles = [
      config.sops.secrets.openvpn-credentials.path
    ];
    environment =
      {
        GLOBAL_APPLY_PERMISSIONS = "false";
        LOCAL_NETWORK = "192.168.10.0/24";
        CREATE_TUN_DEVICE = "true";
        PEER_DNS = "true";
        PEER_DNS_PIN_ROUTES = "true";

        OPENVPN_PROVIDER = "SURFSHARK";
        OPENVPN_CONFIG = "ch-zur.prod.surfshark.com_udp,fr-mrs.prod.surfshark.com_udp";
        PGID = builtins.toString config.users.groups."users".gid;
        PUID = builtins.toString config.users.users."pengu".uid;

        TRANSMISSION_DOWNLOAD_DIR = completeDir;
        TRANSMISSION_DOWNLOAD_QUEUE_ENABLED = "true";
        TRANSMISSION_DOWNLOAD_QUEUE_SIZE = "20";
        TRANSMISSION_HOME = configDir + "/transmission-home";
        TRANSMISSION_INCOMPLETE_DIR = incompleteDir;
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

  sops.secrets."openvpn-credentials" = {
    owner = config.users.users."pengu".name;
  };
}
