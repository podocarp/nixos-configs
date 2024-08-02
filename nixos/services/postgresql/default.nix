{ port, ... }:
{
  services.postgresql = {
    enable = true;
    dataDir = "/tank/local/postgresql";
    settings = {
      port = port;
    };
    ensureUsers = [
      {
        name = "gitea";
        ensureDBOwnership = true;
      }
      {
        name = "grafana";
        ensureDBOwnership = true;
      }
    ];
    ensureDatabases = [
      "gitea"
      "grafana"
    ];
    authentication = ''
      #TYPE     DATABASE      USER      ADDRESS     METHOD
      local     all           all                   trust
    '';
  };
}
