{ config, pkgs, lib, ... }:

{
  imports = [
    <home-manager/nixos>
    ./common.nix
    ../misc/xserver.nix
  ];

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

  # to allow for gtk theme config
  programs.dconf.enable = true;

  services.xserver.videoDrivers = [ "nvidia" ];

  # Some hardware acceleration things.
  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      nvidia-vaapi-driver
      vaapiVdpau
    ];
  };

  hardware.nvidia = {
    powerManagement.enable = true;
  };

  environment.variables = {
    "LIBVA_DRIVER_NAME" = "vdpau";
    "VDPAU_DRIVER" = "nvidia";
  };

  systemd.services.fix_acpi_wakeup = {
    serviceConfig.Type = "oneshot";
    description = "Prevents USB from waking the system up so the system can sleep properly.";
    script = ''
      ${pkgs.bash}/bin/bash -c 'echo PTXH > /proc/acpi/wakeup; echo XHC0 > /proc/acpi/wakeup;'
    '';
    wantedBy = [ "multi-user.target" ];
  };

  virtualisation.virtualbox.host.enable = true;
  users.extraGroups.vboxusers.members = [ "pengu" ];

  home-manager.users.pengu = import ../home-manager/desktop.nix;
  # services.xserver.desktopManager.plasma5.enable = true;

  sound.enable = true;
  hardware.pulseaudio.enable = true;
}
