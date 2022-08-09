{ port, dbPort, ... }:
let
  postgresPort = toString dbPort;
  socketPath = "/var/run/postgresql";
in
{
  services.hydra = {
    enable = true;
    hydraURL = "hydra.jiaxiaodong.com";
    port = port;
    notificationSender = "hydra@localhost";
    buildMachinesFiles = [];
    useSubstitutes = true;
    dbi = "dbi:Pg:dbname=hydra;user=hydra;host=${socketPath};port=${postgresPort}";
    extraConfig = ''
      using_frontend_proxy 1
    '';
  };

  systemd.services."hydra-init.service".after = [ "postgresql.service" ];
}
