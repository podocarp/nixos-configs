{ pkgs, lib, ... }:

{
  boot.loader.grub = {
    useOSProber = true;
  };
  boot.extraModprobeConfig = ''
  '';

  networking.hostName = "desktop"; # Define your hostname.
  networking.firewall.allowedUDPPorts = [ 50000 ];
  networking.wireguard = {
    enable = false;
    interfaces = {
      wg0 = {
        ips = [ "10.0.0.1/24" ];
        listenPort = 50000;

        privateKey =
          builtins.elemAt 0
            (lib.strings.splitString "\n"
              (builtins.extraBuiltins.getSecret "nix/wireguard_client"));

        peers = [
          {
            publicKey = "EnNBgGNhYPEWP+eb/uy4Ye4/YCxFCgy1kMQtb+H/yw4=";
            allowedIPs = [ "0.0.0.0/0" ];
            persistentKeepalive = 25;
          }
        ];
      };
    };
  };

  # nixpkgs.config.packageOverrides = pkgs: {
  # };

  services.xserver.videoDrivers = [ "nvidia" ];

  # Some hardware acceleration things.
  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  virtualisation.virtualbox.host.enable = true;
  users.extraGroups.vboxusers.members = [ "pengu" ];

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  system.stateVersion = "20.09";
}
