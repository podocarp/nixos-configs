{ lib, port, ... }:
{
  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    group = "users";
    guiAddress = "localhost:${toString port}";

    devices = {
      desktop = {
        id = "522AIVA-4BGC4VL-2E3GHJJ-F3W2P4P-2U2BXJX-ZMVI2IS-SIFD4VM-6EZ7PAP";
      };
      x200 = {
        id = "65LWGSU-4PZP5YT-7MOTAGN-QILV4Q6-XXQ2UOG-DP24BML-TPS7YVS-YJJKHAF";
      };
    };

    folders = {
      "/tank/public/School" = {
        id = "School";
        rescanInterval = 36000;
        versioning.type = "trashcan";
        versioning.params = { "cleanoutDays" = "7"; };
        devices = [ "desktop" "x200" ];
      };
      "/tank/public/Books" = {
        id = "Books";
        rescanInterval = 36000;
        devices = [ "desktop" "x200" ];
      };
    };

    extraOptions = {
      gui = {
        insecureSkipHostcheck = true;
      } //
      (# file has format: <pass>\n---\nbcrypt: <hash>\nuser: <user>\nblablabla
      let
        xs = lib.strings.splitString "\n"
          (builtins.extraBuiltins.getSecret "nix/syncthing");
      in
      {
        password = lib.strings.removePrefix "bcrypt: " (builtins.elemAt xs 2);
        user = lib.strings.removePrefix "user: " (builtins.elemAt xs 3);
      });

      options = {
        urAccepted = -1;
        autoUpdateIntervalH = 0;
        defaultFolderPath = "";
      };
    };
  };
}
