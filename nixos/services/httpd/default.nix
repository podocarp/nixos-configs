{ portMap, ... }:
{
  services.httpd = {
    enable = true;
    adminAddr = "xdjiaxd@gmail.com";
    extraModules = [
      "proxy"
      "proxy_http"
      # "proxy_wstunnel" only for websocket tunneling
    ];

    virtualHosts = builtins.listToAttrs (map
    (xs:
    let
      name = "${builtins.elemAt xs 0}.jiaxiaodong.com";
      port = toString (builtins.elemAt xs 1);
    in
    {
      name = name;
      value = {
        hostName = name;
        serverAliases = [ name ];

        robotsEntries = ''
          User-agent: *
          Disallow: /
        '';

        extraConfig = ''
          ProxyPreserveHost On
          <Proxy *>
              Order allow,deny
              Allow from all
          </Proxy>
          ProxyPass / http://localhost:${port}/
          ProxyPassReverse / http://localhost:{port}/
        '';
      };
    }) portMap
    );
  };
}
