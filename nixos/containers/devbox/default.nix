{ devboxPort, config, pkgs, ... }:
let
  exposedPort = 4567;
in
{
  imports = [
  ];

  virtualisation.oci-containers.containers."firefly" = {
    autoStart = true;
    ephemeral = false;
    image = "linuxserver/code-server:latest";

    volumes = [
    ];

    ports = [
      "${toString devboxPort}:${exposedPort}"
    ];

    environment = {
      PORT = exposedPort;
      TZ = "Asia/Singapore";
    };

    extraOptions = [ "--gpus=all" "--runtime=nvidia" ];
  };
}
