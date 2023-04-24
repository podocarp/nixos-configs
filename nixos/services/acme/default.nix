{ config, ... }:
{
  security.acme = {
    acceptTerms = true;
    defaults.email = "xdjiaxd@gmail.com";

    certs."wildcard-jiaxiaodong-com" = {
      domain = "*.jiaxiaodong.com";
      dnsProvider = "cloudflare";
      credentialsFile = config.sops.secrets.acme-credentials.path;
      dnsPropagationCheck = true;
      enableDebugLogs = true;
    };
  };

  sops.secrets.acme-credentials = {
    owner = "acme";
  };
}
