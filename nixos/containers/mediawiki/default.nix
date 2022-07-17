{ config, pkgs, port, ... }:
let
    pwdfilePath = "/tmp/password";
in
{
  containers.mediawiki= {
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
      services.mediawiki= {
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
            url = "https://extdist.wmflabs.org/dist/extensions/InputBox-REL1_36-aa70764.tar.gz";
            sha256 = "0rqvn6rf7jswv02s8ajh31hd5ksvxq84sjpapda0083drxbwj96f";
          };
        };

        extraConfig = ''
          $wgDefaultUserOptions['gender'] = 'male';
        '';

        virtualHost = {
            hostName = "wiki.home.com";
            adminAddr = "xdjiaxd@gmail.com";

            listen = [
              {
                "ip" = "*";
                "port" = port;
                # "ssl" = true;
              }
            ];
        };
      };

      system.stateVersion = "22.11";
    };
  };

  sops.secrets."mediawiki" = {};
}
