{ prometheusPort
, nodeExporterPort
, zfsExporterPort
, nginxExporterPort
, nginxLogExporterPort
, ...
}:
{
  services.prometheus = {
    enable = true;
    port = prometheusPort;
    exporters = {
      node = {
        enable = true;
        enabledCollectors = [ "systemd" ];
        port = nodeExporterPort;
      };

      zfs = {
        enable = true;
        port = zfsExporterPort;
      };

      nginx = {
        enable = true;
        sslVerify = false;
        port = nginxExporterPort;
      };

      nginxlog = {
        enable = true;
        group = "nginx";
        port = nginxLogExporterPort;
        settings = {
          namespaces = [
            {
              name = "nginx";
              format = "$remote_addr - $remote_user [$time_local] \"$request\" $status $body_bytes_sent \"$http_referer\" \"$http_user_agent\" \"$http_x_forwarded_for\"";
              source = { files = [ "/var/log/nginx/access.log" ]; };
              histogram_buckets = [ .005 .01 .025 .05 .1 .25 .5 1 2.5 5 10 ];
            }
          ];
        };
      };
    };

    globalConfig.scrape_interval = "30s";

    scrapeConfigs = [
      {
        job_name = "obsidian";
        static_configs = [{
          targets = [
            "localhost:${toString nodeExporterPort}"
            "localhost:${toString zfsExporterPort}"
            "localhost:${toString nginxExporterPort}"
            "localhost:${toString nginxLogExporterPort}"
          ];
        }];
      }
      {
        job_name = "openwrt";
        static_configs = [{
          targets = [ "openwrt.lan:9100" ];
        }];
      }
    ];

    rules = [
      ''
        groups:
          - name: stats
            rules:
            - record: job:cpu_total_cores
              expr: (count by (job) (sum by (cpu, job) (node_cpu_seconds_total)))
            - record: job_device:filesystem_avail_bytes:sum
              expr: sum by (device, job) (node_filesystem_avail_bytes{device!="ramfs", device!="none", device!="tmpfs"})
            - record: job_device:filesystem_size_bytes:sum
              expr: sum by (device, job) (node_filesystem_size_bytes{device!="ramfs", device!="none", device!="tmpfs"})
            - record: job_instance:node_memory_CacheBuffers_bytes
              expr: node_memory_Cached_bytes + node_memory_Buffers_bytes + node_memory_SReclaimable_bytes
            - record: job_instance:node_memory_Used_bytes
              expr: node_memory_MemTotal_bytes - node_memory_MemFree_bytes - job_instance:node_memory_CacheBuffers_bytes
      ''
    ];
  };
}



