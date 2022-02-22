{ port, sshPort, ... }:
{
  # This is a private network only instance. We do not open the ssh ports in
  # the system firewall.
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
        domain = "gitea.home.com";
        rootUrl = "https://gitea.home.com";
        httpPort = port;
        ssh.clonePort = sshPort;

        settings = {
          server = {
            START_SSH_SERVER = true;
            SSH_DOMAIN = "%(DOMAIN)s";
            LANDING_PAGE = "login";
          };
        };
      };
    };
  };
}
