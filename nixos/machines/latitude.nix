{ config, pkgs, libs, ... }:

{
  imports =
    [
      ./common.nix
      ../misc/xserver.nix
      ../containers/elasticsearch/default.nix
      <home-manager/nixos>
      <sops-nix/modules/sops>
    ];

  boot.loader.grub = {
    efiSupport = true;
    efiInstallAsRemovable = true;
    device = "nodev";
  };

  sops = {
    defaultSopsFile = ../secrets/work_secrets.yaml;
    gnupg.sshKeyPaths = [];
    age.keyFile = "/var/lib/sops/age/keys.txt";
  };

  boot.extraModprobeConfig = ''
    options iwlwifi power_save=1
    options hid_apple fnmode=0
  '';

  networking.hostName = "work"; # Define your hostname.

  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };

  services.xserver.videoDrivers = [ "modesetting" ];
  # Some hardware acceleration things.
  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      intel-media-driver # LIBVA_DRIVER_NAME=iHD
      vaapiIntel         # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  home-manager.users.pengu = import ../home-manager/work_laptop.nix;

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  services.xserver.dpi = 100;
  services.xserver.libinput = {
    enable = true;
    touchpad.accelSpeed = "0.3";
  };

  virtualisation.virtualbox.host.enable = true;
  users.extraGroups.vboxusers.members = [ "pengu" ];
  virtualisation.docker.enable = true;
}
