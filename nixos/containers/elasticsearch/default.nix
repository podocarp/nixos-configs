{ ... }:
{
  virtualisation.oci-containers.containers."elasticsearch" = {
    autoStart = false;
    image = "docker.elastic.co/elasticsearch/elasticsearch:6.8.2";
    ports = [ "9200:9200" "9300:9300" ];
    environment = {
      "discovery.type" = "single-node";
    };
  };
}
