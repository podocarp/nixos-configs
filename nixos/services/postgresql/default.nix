{ port, ... }:
{
  services.postgresql = {
    enable = true;
    port = port;
    dataDir = "/tank/local/postgresql";
    ensureUsers = [
      {
        name = "hydra";
        ensurePermissions = {
          "DATABASE hydra" = "ALL PRIVILEGES";
        };
      }
      {
        name = "gitea";
        ensurePermissions = {
          "DATABASE gitea" = "ALL PRIVILEGES";
        };
      }
    ];
    ensureDatabases = [
      "hydra"
      "gitea"
    ];
    authentication = ''
      #TYPE     DATABASE      USER      ADDRESS     METHOD
      local     all           all                   trust
    '';
  };
}
