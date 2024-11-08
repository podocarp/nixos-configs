{ config, ... }:
{
  networking.wireless = {
    userControlled.enable = true;
    enable = true;
    environmentFile = config.sops.secrets."network_env".path;
    networks = {
      "Wireless@SGx" = {
        auth = ''
          key_mgmt=WPA-EAP
          eap=PEAP
          identity="@WSG_IDENTITY@"
          password="@WSG_PASSWORD@"
        '';
      };
      "potato" = {
        psk = "@POTATO_PSK@";
      };
      "mobile potato" = {
        psk = "@POTATO_PSK@";
      };
    };
  };

  sops.secrets."network_env" = {
    sopsFile = ../../secrets/secrets-network.yaml;
  };

}
