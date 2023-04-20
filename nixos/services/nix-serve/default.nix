{ config, port, ... }:
{
  services.nix-serve = {
    enable = true;
    port = port;
    secretKeyFile = config.sops.secrets.cache-priv-key.path;
  };

  sops.secrets.cache-priv-key = { mode = "0440"; };
}
