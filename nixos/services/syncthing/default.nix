{ guiport, ... }:
{
  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    group = "users";
    extraOptions = {
      gui = {
        insecureSkipHostcheck = true;
      };
    };
  };
}
