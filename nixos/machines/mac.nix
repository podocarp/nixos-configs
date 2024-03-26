args@{ pkgs, ... }:
{
  imports = [
    ((import ../services/yabai) args)
    ../services/skhd
  ];

  users.users.bytedance = {
    home = "/Users/bytedance";
  };

  home-manager.users.bytedance = import ../home-manager/mac.nix;

  environment.systemPackages = with pkgs; [
  ];

  environment.variables = {
    LC_ALL = "en_US.UTF-8";
    LANG = "en_US.UTF-8";
  };

  networking.wg-quick = {
    interfaces.wg0 = {
      autostart = false;
      address = [
        "10.0.0.20/32"
      ];
      dns = [ "192.168.1.1" ];
      # ArmHlIa/tastlR0PuC0GQJjr7k4wyyhaIkbnVUCNWRo=
      privateKeyFile = "/tmp/test.txt";
      peers = [
        {
          allowedIPs = [
            "0.0.0.0/0"
          ];
          endpoint = "hs.jiaxiaodong.com:4500";
          publicKey = "1LIxpQZck/+GR9G675ijuGEFAF4AK0/sApE84yTcQS0=";
        }
      ];
    };
  };

  nixpkgs.hostPlatform = "aarch64-darwin";
  nixpkgs.config.allowUnfree = true;
  nix.useDaemon = true;

  nix = {
    settings = {
      experimental-features = [ "flakes" "nix-command" ];
    };
  };
}
