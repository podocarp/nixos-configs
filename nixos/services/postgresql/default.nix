{ port, ... }:
{
  services.postgresql = {
    enable = true;
    port = port;
    dataDir = "/tank/local/postgresql";
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
