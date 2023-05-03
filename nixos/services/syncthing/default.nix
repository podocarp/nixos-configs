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
        id = "N7K4BTU-RRSRD7G-G6VLXUE-GHNX7YT-TF4B6HB-7HTAWE2-2R2IYAA-UXMLQQN";
      };
      xperia = {
        id = "M2Z3RWZ-K7FGIZM-E5IH5HT-OUGIOS3-V73IOTO-H7WFDMN-FGTBPU3-RIRW3Q4";
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
        devices = [ "desktop" "x200" "x1" "slate" "xperia" ];
      };
      "/tank/public/Sync/Keepass" = {
        id = "Keepass";
        rescanInterval = 36000;
        devices = [ "desktop" "x200" "x1" "slate" "xperia" ];
      };
    };

    extraOptions = {
      gui = {
        enabled = true;
        insecureSkipHostcheck = true;
        insecureAdminAccess = false;
      };

      options = {
        urAccepted = -1;
        autoUpdateIntervalH = 0;
        defaultFolderPath = "";
      };
    };
  };
}
