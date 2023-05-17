{ dcgmExporterPort, ... }:
{
  virtualisation.oci-containers.containers."dcgm-exporter" = {
    autoStart = true;
    image = "nvcr.io/nvidia/k8s/dcgm-exporter:3.1.7-3.1.4-ubuntu20.04";
    volumes = [ ];
    ports = [ "${toString dcgmExporterPort}:9400" ];
    extraOptions = [ "--gpus=all" "--runtime=nvidia" ];
  };
}
