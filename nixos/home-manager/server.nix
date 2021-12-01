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

  imports = [
    ((import ./programs/vifm/default.nix) {
      myTerm = myTerm;
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
    telnet
    unzip
    usbutils # for lsusb
    veracrypt
    vifm
    zip
  ];

  xresources.extraConfig =
  ''
      ! PaperColor Theme
      *.foreground: #4D4D4C
      *.background: #FFFFFF
      ! black
      *.color0: #EDEDED
      *.color8: #969694
      ! red
      *.color1: #D7005F
      *.color9: #D7005F
      ! green
      *.color2: #718C00
      *.color10: #718C00
      ! yellow / orange
      *.color3: #D75F00
      *.color11: #D75F00
      ! blue
      *.color4: #4271AE
      *.color12: #4271AE
      ! magenta
      *.color5: #8959A8
      *.color13: #8959A8
      ! cyan
      *.color6: #3E999F
      *.color14: #3E999F
      ! white
      *.color7: #F5F5F5
      *.color15: #2D2D2C
  '';
}

