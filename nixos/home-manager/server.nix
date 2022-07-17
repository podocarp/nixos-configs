{ config, pkgs, lib, ... }:

let
  # It is important to change this when needed. This is a global setting for
  # many other configs.
  myTerm = "xterm";
  homeDir = "/home/pengu";
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "pengu";
  home.homeDirectory = homeDir;
  home.stateVersion = "22.11";

  imports = [
    ((import ./programs/vifm/default.nix) {
      pkgs = pkgs; myTerm = myTerm;
    })
    ((import ./programs/password-store/default.nix) {
      homeDir = homeDir;
    })

    ./programs/bash/default.nix
    ./programs/git/default.nix
    ./programs/gpg/default.nix
    ./programs/neovim/default.nix
    ./programs/readline/default.nix
    ./programs/tmux/default.nix

    ./services/gpg-agent/default.nix

    ./scripts/default.nix

    ./misc/xresources/default.nix
  ];

  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
    ### Applications
    bind # for nslookup
    gcc
    gdb
    gnumake
    highlight
    iftop
    inetutils # telnet
    iotop
    lm_sensors
    neofetch
    nmap
    openvpn
    (python3.withPackages(p: with p; [
      pip
    ]))
    stress
    sysstat
    unzip
    usbutils # for lsusb
    veracrypt
    zip
  ];
}

