{ dir, ... }:
{
  security.acme = {
    email = "xdjiaxd@gmail.com";
    acceptTerms = true;
  };

  services.httpd.virtualHosts."root"= {
    hostName = "hs.jiaxiaodong.com";
    documentRoot = dir;
  };

  systemd.tmpfiles.rules = [
    "d ${dir} 0777 root root"
  ];

}
