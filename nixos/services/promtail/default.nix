{ lokiPort, ... }:
{
  services.promtail = {
    enable = true;
    configuration = {
      server = {
        disable = true;
      };

      positions.filename = "/tmp/promtail_positions.yaml";

      clients = [
        { url = "http://localhost:${toString lokiPort}/loki/api/v1/push"; }
      ];

      scrape_configs = [
        {
          job_name = "journal";
          journal = {
            max_age = "12h";
            labels = {
              job = "systemd-journal";
              host = "obsidian";
            };
          };
          relabel_configs = [
            {
              source_labels = [ "__journal__systemd_unit" ];
              target_label = "unit";
            }
          ];
        }
      ];
    };
  };
}
