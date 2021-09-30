{ portMap, ... }:
{
  services.nginx= {
    enable = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;

    virtualHosts =
    let
      # Takes a list of [name, port] into config that we want
      configgen = (host: xs:
        let
          name = builtins.elemAt xs 0;
          port = toString (builtins.elemAt xs 1);
        in
        {
          name = "${name}.${host}";
          value = {
            locations."/".proxyPass = "http://localhost:${port}";
          };
        }
      );
    in
    # portMap is a list of lists of [name, port]
    # for each hostname, we generate the config for name.hostname in the foldl
    (builtins.foldl'
      (x: y: x // builtins.listToAttrs (map (configgen y) portMap))
      {}
      [ "home.com" "jiaxiaodong.com" ]
    );
  };
}
