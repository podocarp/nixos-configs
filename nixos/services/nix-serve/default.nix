{ config, port, ... }:
{
  services.nix-serve = {
    enable = true;
    port = port;
    secretKeyFile = config.sops.secrets.cache-priv-key.path;
    extraParams = "--error-log /var/log/nix-serve.log -pid /var/run/nix-serve.pid";
  };

  sops.secrets.cache-priv-key = { mode = "0440"; };
}
