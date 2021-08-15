{ ... }:
{
  services.fail2ban = {
    enable = true;
    maxretry = 5;
    bantime-increment = {
      enable = true;
      maxtime = "48h";
    };

    # sshd jail is included by default
    jails = {
      apache-nohome = ''
        # Block an IP address if it accesses a non-existent home directory more
        # than 5 times in 10 minutes
        filter   = apache-nohome
        action   = iptables-multiport[name=HTTP, port="http,https"]
        logpath  = /var/log/httpd/*error.log
        findtime = 600
        maxretry = 5
      '';
      apache-badbots= ''
        # Blocks malicious bot request patterns
        filter   = apache-badbots
        action   = iptables-multiport[name=HTTP, port="http,https"]
        logpath  = /var/log/httpd/*error.log
        maxretry = 2
      '';
    };
  };
}
