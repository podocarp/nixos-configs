{ port, ... }:
{
  virtualisation.oci-containers.containers."mealie" = {
    autoStart = true;
    image = "hkotel/mealie";
    volumes = [
      "/tank/local/mealie:/app/data"
    ];
    ports = [ "${toString port}:80" ];
    environment = {
      PUID = "1000";
      PGID = "100";
      TZ = "Asia/Singapore";
      RECIPE_PUBLIC = "true";
      RECIPE_SHOW_NUTRITION = "true";
      RECIPE_SHOW_ASSETS = "true";
      RECIPE_LANDSCAPE_VIEW = "true";
      RECIPE_DISABLE_COMMENTS = "false";
      RECIPE_DISABLE_AMOUNT = "false";
      WEB_CONCURRENCY = "1";
      MAX_WORKERS = "2";
    };
  };
}
