{ port, ... }:
{
  services.grafana = {
    enable = true;
    port = port;
    analytics.reporting.enable = false;
  };
}
