{ config, port, ... }:
{
  containers.gollum = {
    autoStart = true;
    ephemeral = true;
    bindMounts = {
      "/var/lib/gollum" = {
        hostPath = "/tank/local/gollum";
        isReadOnly = false;
      };
    };

    config = {
      services.gollum = {
        enable = true;
        stateDir = "/var/lib/gollum";
        port = port;

        extraConfig = ''
Gollum::Hook.register(:post_commit, :hook_id) do |committer, sha1|
  committer.wiki.repo.git.pull('origin', committer.wiki.ref)
  committer.wiki.repo.git.push('origin', committer.wiki.ref)
end
        '';
      };
    };
  };
}
