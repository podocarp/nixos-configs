{ devboxPort, testPort, config, pkgs, ... }:
let
  exposedPort = "4567";
  passwordPath = "/run/secrets/password";
in
{
  imports = [
  ];

  virtualisation.oci-containers.containers."devbox" = {
    autoStart = true;
    image = "my/devbox:latest";

    volumes = [
      "/tank/local/devbox:/home/workspace:cached"
      "/home/pengu/Documents/ML/models:/home/models"
      "${config.sops.secrets."devbox-password".path}:${passwordPath}"
    ];

    ports = [
      "${toString devboxPort}:${exposedPort}"
      "${toString testPort}:${toString testPort}"
    ];

    environment = {
      PORT = toString testPort;
      TZ = "Asia/Singapore";
    };

    cmd = [
      "--connection-token-file=${passwordPath}"
      "--host=0.0.0.0"
      "--port=${exposedPort}"
    ];
    entrypoint = "/home/.openvscode-server/bin/openvscode-server";
    extraOptions = [ "--gpus=all" "--runtime=nvidia" ];
  };

  sops.secrets."devbox-password" = {
    owner = "pengu";
  };
}
