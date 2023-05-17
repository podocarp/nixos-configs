{ lokiPort, ... }:
{
  services.loki = {
    enable = true;
    dataDir = "/tank/local/loki";
    configuration = {
      auth_enabled = false;
      analytics.reporting_enabled = false;

      server = {
        http_listen_port = lokiPort;
      };

      common = {
        instance_addr = "127.0.0.1";
        path_prefix = "/tank/local/loki";
        replication_factor = 1;
        ring.kvstore.store = "inmemory";
      };
      schema_config = {
        configs = [{
          from = "2023-01-01";
          store = "boltdb-shipper";
          object_store = "filesystem";
          schema = "v11";
          index = {
            prefix = "index_";
            period = "24h";
          };
        }];
      };

    };
  };
}
