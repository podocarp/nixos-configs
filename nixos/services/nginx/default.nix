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
        customConfigs = {
          "error.jiaxiaodong.com".locations."/" = {
            return = "404";
            priority = 1;
          };
        };

        # Maps a list [name, port, public] into a config that we want
        configgen = (host: isPublicDomain: xs:
          let
            name = builtins.elemAt xs 0;
            port = toString (builtins.elemAt xs 1);
            openToPublic = builtins.elemAt xs 2;
            # skip if it's a public domain and you don't want it open to public
            skip = if isPublicDomain then !openToPublic else false;
          in
          {
            name = "${name}.${host}";
            value = if skip then { } else
            {
              locations."/" = {
                proxyPass = "http://localhost:${port}";
                proxyWebsockets = true;
                priority = 1000; # lower is higher
                extraConfig = ''
                  access_log /var/log/nginx/access.log custom;
                '';
              };
              forceSSL = true;
              # addSSL = true;
            } // (
              if isPublicDomain then {
                enableACME = true;
              } else {
                enableACME = false;
                sslCertificate = config.sops.secrets.tls_cert.path;
                sslCertificateKey = config.sops.secrets.tls_key.path;
              }
            );
          }
        );
        publicHosts = builtins.listToAttrs (
          map (configgen "jiaxiaodong.com" true) portMap
        );
        privateHosts = builtins.listToAttrs (
          map (configgen "home.lan" false) portMap
        );
      in
      publicHosts // privateHosts // customConfigs;
  };

  sops.secrets.tls_cert.owner = nginxUsername;
  sops.secrets.tls_key.owner = nginxUsername;
}
