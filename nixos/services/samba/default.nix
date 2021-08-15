{ ... }:
{
  services.samba = {
    enable = true;
    securityType = "user";
    shares.public = {
      path = "/tank/public";
      writeable = "yes";
      browseable = "yes";
      "force user" = "fileshare";
      "force group" = "users";
      "create mask" = "0774";
      "force create mode" = "0774";
      "directory mask" = "2774";
      "force directory mode" = "2774";
    };
    shares.private= {
      path = "/tank/private";
      writeable = "yes";
      browseable = "yes";
      "valid users" = "pengu";
      "force user" = "pengu";
      "create mask" = "0700";
      "force create mode" = "0700";
      "directory mask" = "2700";
      "force directory mode" = "2700";
    };
    shares.global = {
      "usershare path" = "/var/lib/samba/usershares";
      "usershare max shares" = "100";
      "usershare allow guests" = "yes";
      "usershare owner only" = "no";
      "server min protocol" = "SMB2_02";
      "socket options" = "TPC_NODELAY IPTOS_LOWDELAY";
    };

    extraConfig = ''
      load printers = no
      printing = bsd
      printcap name = /dev/null
      disable spoolss = yes
    '';
  };

  networking.firewall.allowedTCPPorts = [ 137 138 139 445 ];
}
