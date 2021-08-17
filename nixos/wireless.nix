{ ... }:

{
  networking.wireless = {
    enable = true;
    userControlled.enable = true;
    networks = {
      "potato" = {
        pskRaw = "b669ba9875b3bf734052b11fcab0856661c2cab652867dd0d280a195041fa00be";
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
