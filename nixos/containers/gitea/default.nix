{ config, port, sshPort, ... }:
{
  containers.gitea = {
    autoStart = true;
    ephemeral = true;
    bindMounts = {
      "/var/lib/gitea" = {
        hostPath = "/tank/local/gitea";
        isReadOnly = false;
      };
    };

    config = {
      services.openssh = {
        enable = true;
        ports = [ sshPort ];
      };

      services.gitea = {
        enable = true;
        domain = "gitea.jiaxiaodong";
        rootUrl = "https://gitea.jiaxiaodong.com";
        httpPort = port;
        ssh.clonePort = sshPort;

        settings = {
          server = {
            SSH_DOMAIN = "ssh.%(DOMAIN)s";
            LANDING_PAGE = "login";
          };
        };
      };
    };
  };
}
