{ config, ... }:
{
  networking.wireless = {
    userControlled.enable = true;
    enable = true;
    secretsFile = config.sops.secrets."network_env".path;
    networks = {
      "Wireless@SGx" = {
        auth = ''
          key_mgmt=WPA-EAP
          eap=PEAP
          identity="ext:WSG_IDENTITY"
          password="ext:WSG_PASSWORD"
        '';
      };
      "potato" = {
        pskRaw = "ext:POTATO_PSK";
      };
      "mobile potato" = {
        pskRaw = "ext:POTATO_PSK";
      };
    };
  };

  sops.secrets."network_env" = {
    sopsFile = ../../secrets/secrets-network.yaml;
  };

}
