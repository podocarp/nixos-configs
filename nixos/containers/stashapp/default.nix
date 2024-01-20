{ port, ... }:
{
  virtualisation.oci-containers.containers."stashapp" = {
    autoStart = true;
    image = "stashapp/stash:v0.23.1";
    volumes = [
      "/tank/private/sets:/media"
      "/tank/local/stashapp:/root/.stash"
    ];
    ports = [ "${toString port}:${toString port}" ];
    environment = {
      STASH_STASH = "/media/";
      STASH_PORT = toString port;
      STASH_EXTERNAL_HOST = "https://stash.home.lan";
    };
  };
}
