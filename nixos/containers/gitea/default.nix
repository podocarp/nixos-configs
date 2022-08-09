{ port, sshPort, ... }:
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
      services.gitea = {
        enable = true;
        domain = "gitea.jiaxiaodong.com";
        rootUrl = "https://gitea.jiaxiaodong.com";
        httpPort = port;
        ssh.clonePort = sshPort;

        settings = {
          server = {
            START_SSH_SERVER = true;
            SSH_DOMAIN = "ssh.gitea.jiaxiaodong.com";
            LANDING_PAGE = "login";
          };
        };
      };

      system.stateVersion = "22.11";
    };
  };
}
