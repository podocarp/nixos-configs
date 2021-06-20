{ port, ... }:
let
  dir = "/var/lib/transmission";
in
{
  containers.transmission = {
    autoStart = true;
    ephemeral = true;
    bindMounts = {
      "${dir}" = {
        hostPath = "/tank/private/transmission";
        isReadOnly = false;
      };
    };

    config = {
      services.transmission = {
        enable = true;
        home = dir;
        settings = {
          rpc-bind-address = "0.0.0.0";
          rpc-port = port;
          rpc-host-whitelist = "obsidian,server";
          rpc-whitelist = "192.168.1.*";
        };
      };
    };
  };
}
