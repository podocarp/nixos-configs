{ giteaPort, giteaSshPort, postgresPort, ... }:
let
  pwdfilePath = "/tmp/password";
in
{
  services.gitea = {
    enable = true;
    stateDir = "/tank/local/gitea/";
    domain = "gitea.jiaxiaodong.com";
    rootUrl = "https://gitea.jiaxiaodong.com";
    httpPort = giteaPort;

    lfs.enable = true;

    database = {
      type = "postgres";
      createDatabase = false;
      name = "gitea";
      socket = "/var/run/postgresql";
      port = postgresPort;
    };

    settings = {
      server = {
        START_SSH_SERVER = true;
        SSH_DOMAIN = "ssh.gitea.jiaxiaodong.com";
        LANDING_PAGE = "login";
        SSH_PORT = giteaSshPort;
      };
      log = {
        MODE = "file";
        LEVEL = "Warn";
      };
    };
  };
}
