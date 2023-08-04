{ port, config, pkgs, ... }:
let
  dbuser = "firefly";
  dbpasspath = "/run/secrets/password";
  appkeypath = "/run/secrets/appkey";
in
{
  imports = [
    ((import ../../services/network-bridge/default.nix) {
      name = "firefly-net";
      pkgs = pkgs;
    })
  ];

  virtualisation.oci-containers.containers."firefly" = {
    autoStart = true;
    image = "fireflyiii/core:latest";
    volumes = [
      "/tank/local/firefly:/var/www/html/storage/upload"
      "${config.sops.secrets.fireflydb-password.path}:${dbpasspath}"
      "${config.sops.secrets.fireflydb-appkey.path}:${appkeypath}"
    ];
    ports = [ "${toString port}:8080" ];
    environment = {
      APP_KEY_FILE = appkeypath;
      DB_HOST = "mysql";
      DB_PORT = "3306";
      DB_CONNECTION = "mysql";
      DB_DATABASE = "firefly";
      DB_USERNAME = dbuser;
      DB_PASSWORD_FILE = dbpasspath;
      TZ = "Asia/Singapore";
    };
    dependsOn = [ "firefly_db" ];
    extraOptions = [ "--network=firefly-net" "--name=firefly" ];
  };

  virtualisation.oci-containers.containers."firefly_db" = {
    autoStart = true;
    image = "mariadb";
    volumes = [
      "/tank/local/firefly_db:/var/lib/mysql"
      "${config.sops.secrets.fireflydb-password.path}:${dbpasspath}"
    ];
    environment = {
      MARIADB_RANDOM_ROOT_PASSWORD = "yes";
      MYSQL_DATABASE = "firefly";
      MYSQL_USER = dbuser;
      MYSQL_PASSWORD_FILE = dbpasspath;
    };
    extraOptions = [ "--network=firefly-net" "--name=mysql" ];
  };

  sops.secrets."fireflydb-password" = { };
  sops.secrets."fireflydb-appkey" = { };
}
