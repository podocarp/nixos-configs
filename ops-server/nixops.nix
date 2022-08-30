{
  network = {
    description = "Server";
    enableRollback = true;
    storage.legacy = {
      databasefile = "~/.nixops/deployments.nixops";
    };
  };

  defaults = {
    imports = [ ../nixos/machines/server.nix ];
  };

  machine = { ... }:
  {
    deployment.targetHost = "192.168.1.107";
  };
}
