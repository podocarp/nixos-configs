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

  networking.hostName = "vm"; # Define your hostname.

  # Select internationalisation properties.
  i18n.defaultLocale = "en_SG.utf8";

  home-manager.users.pengu = import ../home-manager/vm.nix;

  nixpkgs.config.allowUnfree = true;

  system.stateVersion = lib.mkForce "22.05";
}
