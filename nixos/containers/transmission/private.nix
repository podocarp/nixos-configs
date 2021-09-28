{ config, lib, port, ... }:
let
  dir = "/data";
in
{
  virtualisation.oci-containers.containers."transmission_private" = {
    autoStart = true;
    image = "haugene/transmission-openvpn";
    volumes = [
      "/tank/local/transmission_priv:${dir}"
      "/tank/private/Downloads:/Downloads"
    ];
    ports = [ "${toString port}:9091" ];
    extraOptions = ["--cap-add=NET_ADMIN"];
    environment =
    # file format:
    # <pass>
    # ---
    # user: <user>
    let
      creds =
        lib.strings.splitString "\n"
          (builtins.extraBuiltins.getSecret "nordvpn");
    in
    {
      PUID = builtins.toString config.users.users."pengu".uid;
      PGID = builtins.toString config.users.groups."users".gid;
      OPENVPN_PROVIDER = "NORDVPN";
      OPENVPN_USERNAME =
        lib.strings.removePrefix "user: " (builtins.elemAt creds 2);
      OPENVPN_PASSWORD = builtins.elemAt creds 0;
      NORDVPN_COUNTRY = "CH";
      NORDVPN_CATEGORY = "standard";
      NORDVPN_PROTOCOL = "UDP";
      LOCAL_NETWORK= "192.168.1.0/24";

      TRANSMISSION_HOME = dir;
      TRANSMISSION_DOWNLOAD_DIR = "/Downloads";
      TRANSMISSION_RPC_HOST_WHITELIST = "*.home.com";
      TRANSMISSION_RPC_WHITELIST = "192.168.1.*,127.0.0.1";
      TRANSMISSION_RPC_AUTHENTICATION_REQUIRED = "true";
      TRANSMISSION_RPC_USERNAME = "pengu";
      TRANSMISSION_RPC_PASSWORD = "pengu";
      TRANSMISSION_PEER_PORT_RANDOM_ON_START = "true";
      TRANSMISSION_DOWNLOAD_QUEUE_ENABLED = "true";
      TRANSMISSION_DOWNLOAD_QUEUE_SIZE = "20";
      TRANSMISSION_QUEUE_STALLED_ENABLED = "true";
      TRANSMISSION_QUEUE_STALLED_MINUTES = "10";
      TRANSMISSION_RATIO_LIMIT_ENABLED = "true";
      TRANSMISSION_RATIO_LIMIT = "1";
    };
  };
}
