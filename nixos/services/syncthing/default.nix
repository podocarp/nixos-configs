{ config, lib, port, ... }:
{
  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    group = "syncthing";
    guiAddress = "localhost:${toString port}";
    overrideFolders = true;
    overrideDevices = true;

    devices = {
      desktop = {
        id = "522AIVA-4BGC4VL-2E3GHJJ-F3W2P4P-2U2BXJX-ZMVI2IS-SIFD4VM-6EZ7PAP";
      };
      x200 = {
        id = "65LWGSU-4PZP5YT-7MOTAGN-QILV4Q6-XXQ2UOG-DP24BML-TPS7YVS-YJJKHAF";
      };
      x1 = {
        id = "OX5NYH3-RHKF6DI-AREQDJV-QLX3W3Q-YIATG65-GCL7N7V-ECT2QMR-WBDCSA4";
      };
      slate = {
        id = "3IY5VEY-HL3E33H-GZ3LJIR-5SIUTLJ-ROZ3FSO-NA5K3X6-JSF4SUE-BXUICAH";
      };
    };

    folders = {
      "/tank/public/Sync/School" = {
        id = "School";
        rescanInterval = 36000;
        versioning.type = "trashcan";
        versioning.params = { "cleanoutDays" = "7"; };
        devices = [ "desktop" "x200" "x1" "slate" ];
      };
      "/tank/public/Sync/Books" = {
        id = "Books";
        rescanInterval = 36000;
        devices = [ "desktop" "x200" "x1" "slate" ];
      };
    };

    extraOptions = {
      gui = {
        insecureSkipHostcheck = true;
        insecureAdminAccess = false;
        user = "syncthing";
        password = "$2y$12$P/sRGMoWp29Q/UveX7MDkepWQoEecl5ABF3W97UVIXCtm1T1XpuZm";
      };

      options = {
        urAccepted = -1;
        autoUpdateIntervalH = 0;
        defaultFolderPath = "";
      };
    };
  };
}
