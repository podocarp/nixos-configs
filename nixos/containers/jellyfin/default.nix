{ ... }:
{
  virtualisation.oci-containers.containers."jellyfin" = {
    autoStart = true;
    image = "jellyfin/jellyfin";
    volumes = [
      "/var/cache/jellyfin/config:/config"
      "/var/cache/jellyfin/cache:/cache"
      "/var/log/jellyfin:/log"
      "/tank/public/Media:/media:ro"
    ];
    ports = [ "8096:8096" ];
    environment = {
      JELLYFIN_LOG_DIR = "/log";
    };
  };
}
