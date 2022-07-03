{ name, pkgs, ... }:
{
  systemd.services."${name}" = {
    description = "docker bridge ${name}";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig.Type = "oneshot";

    script =
    ''
      exists=$(${pkgs.docker}/bin/docker network ls | grep "${name}")
      if [ -z $exists ]; then
        ${pkgs.docker}/bin/docker network create ${name}
      else
        echo "Bridge ${name} already exists!"
      fi
    '';
  };
}
