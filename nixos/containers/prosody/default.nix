{ config, pkgs, ... }:
{
  containers.prosody = {
    autoStart = true;
    ephemeral = true;
    bindMounts = {
      "/ssl" = {
        hostPath = toString ../../ssl;
        isReadOnly = true;
      };
      "/var/lib/prosody" = {
        hostPath = "/tank/local/prosody";
        isReadOnly = false;
      };
    };

    config = {
      services.prosody = {
        enable = true;
        admins = [ "jxd@chat.jiaxiaodong.com" ];
        ssl.cert = "/ssl/host.cert";
        ssl.key = "/ssl/host.key";
        virtualHosts."chat.jiaxiaodong.com" = {
          enabled = true;
          domain = "chat.jiaxiaodong.com";
          ssl.cert = "/ssl/host.cert";
          ssl.key = "/ssl/host.key";
        };
        muc = [ {
          domain = "conference.jiaxiaodong.com";
        } ];
        uploadHttp= {
          domain = "https://upload.jiaxiaodong.com";
        };

        modules.register = true;
        modules.motd = true;
        modules.watchregistrations = true;

        extraConfig = ''
          log = "/var/lib/prosody/prosody.log"
          allow_registration = true
          motd_text = [[Welcome! Type /help -a for a list of commands.]]
        '';
        package = pkgs.prosody.override {
            withCommunityModules = [ "http_upload" ];
        };
      };
    };
  };

  environment.systemPackages = with pkgs; [ prosody ];
}
