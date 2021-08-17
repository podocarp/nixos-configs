{ pkgs, ... }:
{
  virtualisation.oci-containers.containers."tinode-mysql" = {
    autoStart = true;
    image = "mysql:5.7";
    volumes = [
      "/var/log/mysql:/var/log"
      "/tank/local/tinode/mysql:/var/lib/mysql"
    ];
    environment = {
      MYSQL_ALLOW_EMPTY_PASSWORD = "yes";
    };
    extraOptions = [ "--network=tinode-net" "--name=mysql" ];
  };

  virtualisation.oci-containers.containers."tinode-srv" = {
    autoStart = true;
    image = "tinode/tinode-mysql:latest";
    ports = [ "6060:6060" ];
    volumes = let
        conf = ./tinode.conf;
      in
      [
        "${conf}:/tinode.conf"
        "/tank/local/tinode/uploads:/uploads"
        "/var/log/tinode:/var/log"
      ];
    environment = {
      EXT_CONFIG = "/tinode.conf";
      # UPGRADE_DB = "true";
      # RESET_DB = "true";
    };
    extraOptions = [ "--network=tinode-net" ];
  };

  systemd.services.tinode-net = {
    description = "Brings up docker bridge tinode-net";
    after = [ "network.target" ];
    serviceConfig.Type = "oneshot";
    script =
    ''
      ${pkgs.docker}/bin/docker network create tinode-net 2&> /dev/null
    '';
  };

  services.httpd.virtualHosts."chat"= {
    hostName = "chat.jiaxiaodong.com";
    addSSL = true;
    sslServerCert = ../../ssl/host.cert;
    sslServerKey = ../../ssl/host.key;
    extraConfig = ''
      ProxyRequests Off
      ProxyPreserveHost On

      ProxyPass / http://localhost:6060/
      ProxyPassReverse / http://localhost:6060/

      RewriteEngine on
      RewriteCond %{HTTP:UPGRADE} ^WebSocket$ [NC]
      RewriteCond %{HTTP:CONNECTION} ^Upgrade$ [NC]
      RewriteRule .* ws://localhost:6060%{REQUEST_URI} [P]
      '';
  };
}
