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
    };

    folders = {
      "/tank/public/Sync/School" = {
        id = "School";
        rescanInterval = 36000;
        versioning.type = "trashcan";
        versioning.params = { "cleanoutDays" = "7"; };
        devices = [ "desktop" "x200" "x1" ];
      };
      "/tank/public/Sync/Books" = {
        id = "Books";
        rescanInterval = 36000;
        devices = [ "desktop" "x200" "x1" ];
      };
    };

    extraOptions = {
      gui = {
        insecureSkipHostcheck = true;
        insecureAdminAccess = false;
        user = "syncthing";
        password = "$2a$12$EXP3SoCTKv.QwCd8pWwRK.n3oG0Rdv3KLEPgAsdpcgs0N2l/jmGjW";
      };

      options = {
        urAccepted = -1;
        autoUpdateIntervalH = 0;
        defaultFolderPath = "";
      };
    };
  };
}
