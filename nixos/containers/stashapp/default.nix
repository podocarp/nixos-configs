{ port, ... }:
{
  virtualisation.oci-containers.containers."stashapp" = {
    autoStart = true;
    image = "stashapp/stash";
    volumes = [
      "/tank/private/stash:/media"
      "/tank/local/stashapp:/root/.stash"
    ];
    ports = [ "${toString port}:${toString port}" ];
    environment = {
      STASH_STASH= "/media/";
      STASH_PORT=toString port;
    };
  };
}