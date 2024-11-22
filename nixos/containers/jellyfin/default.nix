{ port, ... }:
{
  virtualisation.oci-containers.containers."jellyfin" = {
    autoStart = true;
    image = "jellyfin/jellyfin:10.8.13";
    volumes = [
      "/tank/local/jellyfin/config:/config"
      "/tank/local/jellyfin/cache:/cache"
      "/tank/local/jellyfin/log:/log"
      "/tank/public/Media:/media"
    ];
    ports = [ "${toString port}:8096" ];
    environment = {
      JELLYFIN_LOG_DIR = "/log";
      NVIDIA_VISIBLE_DEVICES = "all";
      NVIDIA_DRIVER_CAPABILITIES = "all";
    };
    extraOptions = [
      "--device=/dev/dri/renderD128"
      # render group gid for vaapi hardware acceleration
      # getent group render | cut -d: -f3
      "--group-add=303"
    ];
  };
}
