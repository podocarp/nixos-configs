{ prometheusPort, nodeExporterPort, ... }:
{
  services.prometheus = {
    enable = true;
    port = prometheusPort;
    exporters.node = {
      enable = true;
      enabledCollectors = [ "systemd" ];
      port = nodeExporterPort;
    };

    scrapeConfigs = [
      {
        job_name = "node_exporter";
        static_configs = [{
          targets = [ "localhost:${toString nodeExporterPort}" ];
        }];
      }
    ];
  };
}
