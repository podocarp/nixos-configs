{ pkgs, dir, ... }:
{
  users.groups."prosody".gid = 149;

  security.acme.certs = {
    "hs.jiaxiaodong.com" = {
      webroot = dir;
      email = "xdjiaxd@gmai.com";
      keyType = "rsa2048";
      group = "prosody";
      extraDomainNames = [
        "chat.jiaxiaodong.com"
        "upload.jiaxiaodong.com"
        "conference.jiaxiaodong.com"
      ];
    };
  };

  containers.prosody = {
    autoStart = true;
    ephemeral = true;
    bindMounts = {
      "/ssl" = {
        hostPath = toString /var/lib/acme/hs.jiaxiaodong.com;
        isReadOnly = true;
      };
      "/var/lib/prosody" = {
        hostPath = "/tank/local/prosody";
        isReadOnly = false;
      };
    };

    config = {
      users.groups."prosody".gid = 149;
      services.prosody = {
        enable = true;
        group = "prosody";
        admins = [ "jxd@chat.jiaxiaodong.com" ];
        ssl.cert = "/ssl/cert.pem";
        ssl.key = "/ssl/key.pem";
        virtualHosts."chat.jiaxiaodong.com" = {
          enabled = true;
          domain = "chat.jiaxiaodong.com";
          ssl.cert = "/ssl/cert.pem";
          ssl.key = "/ssl/key.pem";
        };
        muc = [ {
          domain = "conference.jiaxiaodong.com";
        } ];
        uploadHttp= {
          domain = "upload.jiaxiaodong.com";
          # 90 days
          uploadExpireAfter = "60 * 60 * 24 * 90";
          # 50 MB
          uploadFileSizeLimit = "50 * 1024 * 1024";
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
}
