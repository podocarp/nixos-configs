{ config, port, isPublic ? "true", ... }:
let
  homeDir = "/var/lib/transmission";
  secretsPath = "/var/run/secrets/transmission";
  defaultConfig = ((import ./settings.nix) ({
    config = config;
    homeDir = homeDir;
    port = port;
    secretsPath = secretsPath;
  })).defaultConfig;
in
{
  containers.transmission = {
    autoStart = true;
    ephemeral = true;
    bindMounts = {
      "${homeDir}" = {
        hostPath = "/tank/local/transmission";
        isReadOnly = false;
      };
      "/downloads" = {
        hostPath = "/tank/public/Downloads";
        isReadOnly = false;
      };
      "${secretsPath}" = {
        hostPath = config.sops.secrets.transmission-credentials.path;
        isReadOnly = true;
      };
    };

    config = defaultConfig;
  };

  sops.secrets."transmission-credentials" = {};
}
