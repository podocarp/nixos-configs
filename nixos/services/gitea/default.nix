{ pkgs, config, giteaPort, giteaSshPort, postgresPort, ... }:
{
  services.gitea = {
    enable = true;
    stateDir = "/tank/local/gitea/";

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
        ROOT_URL = "https://gitea.home.lan";
        HTTP_PORT = giteaPort;
        DOMAIN = "gitea.home.lan";
        START_SSH_SERVER = true;
        SSH_DOMAIN = "home.lan";
        SSH_PORT = giteaSshPort;
        LANDING_PAGE = "login";
      };
      log = {
        MODE = "file";
        LEVEL = "Warn";
      };
    };
  };

  systemd.services.gitea-runner-image =
    let
      # reference: https://github.com/nix-community/docker-nixpkgs/blob/master/images/nix/default.nix
      nix-image = pkgs.dockerTools.streamLayeredImage {
        name = "nix-unstable";
        tag = "latest";

        # fromImageName = "nixpkgs/nix-flakes";
        # fromImageTag = "nixos-23.11";

        contents = with pkgs;[
          dockerTools.caCertificates
          dockerTools.fakeNss

          coreutils
          bashInteractive
          nix
          # needed for many actions to run
          nodejs

          gitMinimal
          git-lfs
          gnutar
          gzip
          openssh

          (pkgs.writeTextFile {
            name = "nix.conf";
            destination = "/etc/nix/nix.conf";
            text = ''
              accept-flake-config = true
              experimental-features = nix-command flakes
              build-users-group =
            '';
          })

          (pkgs.writeTextFile {
            name = "ca.crt";
            destination = "/ca.crt";
            text = builtins.readFile ../nginx/ca.crt;
          })
        ];

        extraCommands = ''
          # for /usr/bin/env
          mkdir usr
          ln -s ../bin usr/bin

          # make sure /tmp exists
          mkdir -m 1777 tmp

          # need a HOME
          mkdir -vp root
        '';

        config = {
          Env = [
            "ENV=/etc/profile.d/nix.sh"
            "BASH_ENV=/etc/profile.d/nix.sh"
            "PATH=/usr/bin:/bin:/.nix-profile/bin:${pkgs.nodejs}/bin/"
            "USER=root"
            "GIT_SSL_CAINFO=/ca.crt"
          ];
          Cmd = [ "/bin/sh" ];
        };
      };
    in
    {
      wantedBy = [ "multi-user.target" "gitea-runner-nix.service" ];
      requires = [ "docker.service" ];
      path = [ config.virtualisation.docker.package ];
      script = ''
        ${nix-image} | docker load
      '';

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = false;
      };
    };

  services.gitea-actions-runner.instances =
    {
      "nix" = {
        name = "nix";
        enable = true;
        tokenFile = config.sops.secrets.gitea-token.path;
        url = "http://localhost:${toString giteaPort}";
        labels = [
          "nix:docker://nix-unstable"
        ];
        settings = {
          runner = {
            insecure = true;
            fetch_timeout = "5s";
            fetch_interval = "10s";
            capacity = 2;
          };
        };
      };
    };

  networking.firewall.allowedTCPPorts = [ giteaSshPort ];

  sops.secrets.gitea-token = { };
}
