{ port, ... }:
{
  services.hydra = {
    enable = true;
    hydraURL = "http://localhost:${port}";
    notificationSender = "hydra@localhost";
    buildMachinesFiles = [];
    useSubstitutes = true;
  };
}
