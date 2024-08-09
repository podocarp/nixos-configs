args@{ config, pkgs, lib, ... }:
let
  myTerm = "xterm";
  homeDir = "/home/pengu";
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  home.username = "pengu";
  home.homeDirectory = homeDir;
  home.stateVersion = "22.11";

  nixpkgs.config.allowUnfree = true;

  imports =
    [
      ((import ./programs/password-store) {
        homeDir = homeDir;
      })
      ((import ./programs/rofi) {
        myTerm = myTerm;
      })

      ./programs/bash
      ./programs/chromium
      ./programs/direnv
      ./programs/fzf
      ./programs/git
      ./programs/gpg
      ./programs/mpv
      ./programs/neovim
      # ./programs/ranger
      ./programs/readline
      # ./programs/sioyek
      ./programs/texlive
      ./programs/tmux
      (import ./programs/vifm args)
      ./programs/vscode
      ./programs/zathura

      ./services/gpg-agent
      ./services/syncthing

      ./scripts

      (import ./misc/applications args)
      ./misc/keyboard
      ./misc/xsession
    ];

  home.packages = with pkgs; [
    cudatoolkit
    ddcutil
    handbrake
    imagemagick
    inkscape
    jellyfin-media-player
    keepassxc
    krita
    libreoffice-fresh
    runelite
    tdesktop

    kicad
    openscad
    prusa-slicer

    godot_4

    (python311.withPackages (p: with p; [
      pygments
    ]))
  ];

  programs.man.enable = false;
}
