{ ... }:

{
  networking.wireless = {
    enable = true;
    userControlled.enable = true;
    networks = {
      "ssid" = {
        # run wpa_passphrase ssid password
        pskRaw = "";
      };
      "eduroam" = {
      	auth = ''
	  key_mgmt=WPA-EAP
	  eap=PEAP
	  identity="school email"
	  password="password"
	  phase2="auth=MSCHAPV2"
	'';
      };
    };
  };
}
