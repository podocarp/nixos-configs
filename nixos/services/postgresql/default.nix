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
    ];
    ensureDatabases = [
      "hydra"
    ];
    identMap = ''
      hydra root hydra
    '';
    authentication = ''
      #TYPE     DATABASE      USER      ADDRESS     METHOD
      local     all           all                   trust
    '';
  };
}
