{ config, minioPort, minioUIPort, ... }:
{
  services.minio = {
    enable = true;
    listenAddress = "localhost:${toString minioPort}";
    consoleAddress = "localhost:${toString minioUIPort}";
    configDir = "/tank/local/minio/config";
    dataDir = [ "/tank/local/minio/data" ];
    rootCredentialsFile = config.sops.secrets."minio-credentials".path;
  };

  sops.secrets."minio-credentials" = { owner = "minio"; };
}
