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

      # Maps a list [name, port] into a config that we want
      configgen = (host: xs:
        let
          name = builtins.elemAt xs 0;
          port = toString (builtins.elemAt xs 1);
        in
        {
          name = "${name}.${host}";
          value = {
            locations."/" = {
              proxyPass = "http://localhost:${port}";
              priority = 1000; # lower is higher
            };
            addSSL = true;
            sslCertificate = config.sops.secrets.tls_cert.path;
            sslCertificateKey = config.sops.secrets.tls_key.path;
          };
        }
      );
    in
    # portMap is a list of lists of [name, port]
    # for each hostname, we generate the config for name.hostname
    # then they are all foldl'ed together
    (builtins.foldl'
      (x: y: x // builtins.listToAttrs (map (configgen y) portMap))
      {}
      [ "home.com" "jiaxiaodong.com" ]
    ) // customConfigs;
  };

  sops.secrets.tls_cert.owner = nginxUsername;
  sops.secrets.tls_key.owner = nginxUsername;
}
