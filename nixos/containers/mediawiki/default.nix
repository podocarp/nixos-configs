{ config, pkgs, port, ... }:
let
  pwdfilePath = "/tmp/password";
in
{
  containers.mediawiki = {
    autoStart = true;
    ephemeral = true;
    bindMounts = {
      "/var/www/html/images" = {
        hostPath = "/tank/local/mediawiki/images";
        isReadOnly = false;
      };
      "/var/lib/mediawiki" = {
        hostPath = "/tank/local/mediawiki";
        isReadOnly = false;
      };
      "/var/lib/mysql" = {
        hostPath = "/tank/local/mediawiki/mysql";
        isReadOnly = false;
      };
      "${pwdfilePath}" = {
        hostPath = config.sops.secrets.mediawiki.path;
        isReadOnly = true;
      };
    };

    config = {
      services.mediawiki = {
        enable = true;
        name = "My Wiki";

        passwordFile = pwdfilePath;

        poolConfig = {
          "pm" = "dynamic";
          "pm.max_children" = 16;
          "pm.max_requests" = 100;
          "pm.max_spare_servers" = 2;
          "pm.min_spare_servers" = 1;
          "pm.start_servers" = 1;
        };

        extensions = {
          InputBox = pkgs.fetchzip {
            url = "https://extdist.wmflabs.org/dist/extensions/InputBox-REL1_38-c936e93.tar.gz";
            sha256 = "sha256-fvbHwTUtYxQq00DO2/DqZ30apnZdMESnKnRrmYowzNg=";
          };
        };

        extraConfig = ''
          $wgDefaultUserOptions['gender'] = 'male';
          $wgGroupPermissions['*']['read'] = false;

        '';

        virtualHost = {
          adminAddr = "xdjiaxd@gmail.com";
          listen = [
            {
              "ip" = "127.0.0.1";
              "port" = port;
            }
          ];
        };
      };

      system.stateVersion = "22.11";
    };
  };

  sops.secrets."mediawiki" = { };
}
