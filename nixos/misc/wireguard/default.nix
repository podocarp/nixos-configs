{ pkgs, config, wireguardPort, ... }:
{
  networking.nat.enable = true;
  networking.nat.internalInterfaces = [ "wg0" ];
  networking.firewall = {
    allowedUDPPorts = [ wireguardPort ];
  };

  networking.wireguard.interfaces.wg0 = {
    ips = [ "10.0.0.1/24" ];
    listenPort = wireguardPort;

    # This allows the wireguard server to route your traffic to the internet and hence be like a VPN
    postSetup = ''
      ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -o enp35s0 -j MASQUERADE
    '';
    # This undoes the above command
    postShutdown = ''
      ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -o enp35s0 -j MASQUERADE
    '';

    privateKeyFile = config.sops.secrets.wg_priv.path;

    peers = [
      {
        publicKey = "9eGuXZvzgd4F2FOnQd/4nNx2xXlxUzSM1BbRs0/nIy8=";
        # List of IPs assigned to this peer within the tunnel subnet. Used to configure routing.
        allowedIPs = [ "10.0.0.10/32" ];
      }
      {
        publicKey = "IT3dP+pfiR7PYZJBqpjQ3LiJH5edfZYeZX0wjT62uAc=";
        # List of IPs assigned to this peer within the tunnel subnet. Used to configure routing.
        allowedIPs = [ "10.0.0.15/32" ];
      }
    ];
  };

  sops.secrets.wg_priv = { };
}
