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
      name = builtins.elemAt xs 0;
      port = toString (builtins.elemAt xs 1);
    in
      {
        # * is not a valid name
        name = builtins.replaceStrings ["*"] ["fallback"] name;
        value = {
          serverAliases = [
            "${name}.home.com"
          ];
          extraConfig = ''
            ProxyRequests Off
            ProxyPreserveHost On

            ProxyPass / http://localhost:${port}/
            ProxyPassReverse / http://localhost:${port}/
          '';
        };
      }) portMap
    );
  };
}
