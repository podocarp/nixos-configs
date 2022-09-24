{ config, portMap, ... }:
let
  nginxUsername = "nginx";
in
{
  users.users."${nginxUsername}" = {
    extraGroups = [ config.users.groups.keys.name ];
  };

  services.nginx= {
    enable = true;
    user = nginxUsername;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;

    virtualHosts =
    let
      customConfigs = {
        "*.jiaxiaodong.com".locations."/" = {
          return = "404";
          priority = 10000;
        };
      };

      # Maps a list [name, port, public] into a config that we want
      configgen = (host: isPublic: xs:
        let
          name = builtins.elemAt xs 0;
          port = toString (builtins.elemAt xs 1);
          skip = if isPublic
            then !(builtins.elemAt xs 2)
            else false;
        in
        {
          name = "${name}.${host}";
          value = if skip then {} else {
            locations."/" = {
              proxyPass = "http://localhost:${port}";
              proxyWebsockets = true;
              priority = 1000; # lower is higher
            };
            addSSL = true;
            sslCertificate = config.sops.secrets.tls_cert.path;
            sslCertificateKey = config.sops.secrets.tls_key.path;
          };
        }
      );
      publicHosts = builtins.listToAttrs (
        map (configgen "jiaxiaodong.com" true) portMap
        );
      privateHosts = builtins.listToAttrs (
        map (configgen "home.com" false) portMap
        );
    in
    publicHosts // privateHosts // customConfigs;
  };

  sops.secrets.tls_cert.owner = nginxUsername;
  sops.secrets.tls_key.owner = nginxUsername;
}
