{ port, ... }:
{
  virtualisation.oci-containers.containers."jellyfin" = {
    autoStart = true;
    image = "jellyfin/jellyfin";
    volumes = [
      "/tank/local/jellyfin/config:/config"
      "/tank/local/jellyfin/cache:/cache"
      "/tank/local/jellyfin/log:/log"
      "/tank/public/Media:/media"
    ];
    ports = [ "${toString port}:${toString port}" ];
    environment = {
      JELLYFIN_LOG_DIR = "/log";
    };
  };
}
