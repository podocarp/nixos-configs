{ ... }:
{
  virtualisation.oci-containers.containers."elasticsearch" = {
    autoStart = true;
    image = "docker.elastic.co/elasticsearch/elasticsearch:6.8.2";
    ports = [ "9200:9200" "9300:9300" ];
    volumes = [
      "/tank/local/elasticsearch/data:/usr/share/elasticsearch/data"
      "/tank/local/elasticsearch/logs:/usr/share/elasticsearch/logs"
    ];
    environment = {
      "discovery.type" = "single-node";
    };
  };
}
