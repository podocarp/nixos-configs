{ config, pkgs, lib, ... }:

{
  imports =
    [
      <home-manager/nixos>
      ./common.nix
      ../misc/xserver.nix
    ];

  # Bootloader.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";
  boot.loader.grub.useOSProber = true;

  virtualisation.virtualbox.guest.enable = true;

  services.xserver.desktopManager.plasma5.enable = lib.mkForce false;
  services.xserver.dpi = 180;

  networking.hostName = "vm"; # Define your hostname.

  home-manager.users.pengu = import ../home-manager/vm.nix;

  users.users.pengu = {
    extraGroups = [ "vboxsf" ];
  };

  nixpkgs.config.allowUnfree = true;

  system.stateVersion = lib.mkForce "22.05";
}
