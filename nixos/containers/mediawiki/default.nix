{ lib, port, ... }:
{
  containers.mediawiki= {
    autoStart = true;
    ephemeral = true;
    bindMounts = {
      "/var/lib/mediawiki" = {
        hostPath = "/tank/local/mediawiki";
        isReadOnly = false;
      };
      "/var/lib/mysql" = {
        hostPath = "/tank/local/mediawiki/mysql";
        isReadOnly = false;
      };
    };

    config = {
      services.mediawiki= {
        enable = true;
        name = "My Wiki";

        passwordFile = builtins.toFile "temp"
          (builtins.elemAt
            (lib.strings.splitString "\n"
              (builtins.extraBuiltins.getSecret "server/mediawiki"))
            2);

        poolConfig = {
            "pm" = "dynamic";
            "pm.max_children" = 16;
            "pm.max_requests" = 100;
            "pm.max_spare_servers" = 2;
            "pm.min_spare_servers" = 1;
            "pm.start_servers" = 1;
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
    };
  };
}
