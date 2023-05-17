{ config, grafanaPort, postgresPort, ... }:
{
  services.grafana = {
    enable = true;
    settings = {
      server = {
        domain = "grafana.home.lan";
        http_addr = "127.0.0.1";
        http_port = grafanaPort;
      };
      database = {
        type = "postgres";
        name = "grafana";
        host = "/var/run/postgresql:${toString postgresPort}";
        user = "grafana";
      };
    };
  };
}
