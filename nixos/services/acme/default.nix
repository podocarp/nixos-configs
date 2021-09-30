{ dir, ... }:
{
  security.acme = {
    email = "xdjiaxd@gmail.com";
    acceptTerms = true;

    certs = {
      "hs.jiaxiaodong.com" = {
        email = "xdjiaxd@gmail.com";
        keyType = "rsa2048";
      };
    };
  };

  services.httpd.virtualHosts."root"= {
    hostName = "hs.jiaxiaodong.com";
    documentRoot = dir;
    enableACME = true;
    extraConfig = ''
      <Directory ${dir}>
        Options FollowSymLinks
        AllowOverride None
        Require all granted
      </Directory>
    '';
  };

  systemd.tmpfiles.rules = [
    "d ${dir} 0777 acme acme"
  ];
}
