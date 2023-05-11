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
    ports = [ "${toString port}:8096" ];
    environment = {
      JELLYFIN_LOG_DIR = "/log";
    };
    extraOptions = [ "--gpus=all,capabilities=video" "--runtime=nvidia" ];
  };
}
