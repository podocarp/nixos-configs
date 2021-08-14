{ config, lib, guiPort, ... }:
{
  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    group = "users";
    guiAddress = "localhost:${toString guiPort}";

    devices = {
      desktop = {
        id = "522AIVA-4BGC4VL-2E3GHJJ-F3W2P4P-2U2BXJX-ZMVI2IS-SIFD4VM-6EZ7PAP";
      };
    };

    folders = {
      "/tank/public/School" = {
        id = "School";
        rescanInterval = 36000;
        versioning.type = "trashcan";
        versioning.params = { "cleanoutDays" = "7"; };
        devices = [ "desktop" ];
      };
    };

    extraOptions = {
      device = {
        id = "E56JZUG-GDNH6ZD-CN5SEJJ-P7F7UBT-R34777D-ZMN7CAA-OILU4JL-BCIGUQC";
      };

      gui = {
        insecureSkipHostcheck = true;
      } //
      (# file has format: pass\n---\nuser: <user>\nblablabla
      let
        xs = lib.strings.splitString "\n"
          (builtins.extraBuiltins.getSecret "nix/syncthing");
      in
      {
        user = lib.strings.removePrefix "user: " (builtins.elemAt xs 2);
        pass = builtins.elemAt xs 0;
      });

      options = {
        urAccepted = -1;
        autoUpdateIntervalH = 0;
        defaultFolderPath = "";
      };
    };
  };
}
