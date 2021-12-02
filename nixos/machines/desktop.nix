{ pkgs, lib, ... }:

{
  imports = [ <home-manager/nixos> ];

  boot.loader.grub = {
    useOSProber = true;
    efiSupport = true;
    device = "nodev";
  };
  boot.loader.efi.canTouchEfiVariables = true;
  boot.extraModprobeConfig = ''
  '';

  networking.hostName = "desktop"; # Define your hostname.
  networking.firewall.allowedUDPPorts = [ 50000 ];

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

  programs.ssh.extraConfig = ''
    HostKeyAlgorithms ssh-rsa
    PubkeyAcceptedKeyTypes ssh-rsa
  '';

  virtualisation.virtualbox.host.enable = true;
  users.extraGroups.vboxusers.members = [ "pengu" ];
  home-manager.users.pengu = import ../home-manager/desktop.nix;

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  system.stateVersion = "20.09";
}
