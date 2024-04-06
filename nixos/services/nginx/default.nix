{ config, portMap, ... }:
let
  nginxUsername = "nginx";
in
{
  users.users."${nginxUsername}" = {
    extraGroups = [ config.users.groups.keys.name ];
  };

  services.nginx = {
    enable = true;
    user = nginxUsername;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    clientMaxBodySize = "5000m";
    statusPage = true;

    commonHttpConfig = ''
      log_format custom '[$time_local] $remote_addr - $remote_user '
      '"$request" $status $body_bytes_sent $request_time $request_length '
      '$upstream_response_time "$http_referer"';
    '';

    virtualHosts =
      let
        # Maps a list [name, port, public] into a config that we want
        configgen = (host: isPublicDomain: xs:
          let
            name = builtins.elemAt xs 0;
            port = toString (builtins.elemAt xs 1);
          in
          {
            name = "${name}.${host}";
            value = {
              locations."/" = {
                proxyPass = "http://localhost:${port}";
                proxyWebsockets = true;
                priority = 1000; # lower is higher
                extraConfig = ''
                  access_log /var/log/nginx/access.log custom;
                '';
              };
              forceSSL = false;
              addSSL = true;
              sslCertificate = config.sops.secrets.home-lan-cert.path;
              sslCertificateKey = config.sops.secrets.home-lan-key.path;
            };
          }
        );
      in
      builtins.listToAttrs (
        map (configgen "home.lan" false) portMap
      );
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  sops.secrets.home-lan-key = {
    owner = nginxUsername;
    sopsFile = ../../secrets/secrets-certs.yaml;
  };
  sops.secrets.home-lan-cert = {
    owner = nginxUsername;
    sopsFile = ../../secrets/secrets-certs.yaml;
  };
}
