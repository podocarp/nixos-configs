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

  imports =
    [
      ((import ./programs/rofi) {
        myTerm = myTerm;
      })
      ((import ./programs/password-store) {
        homeDir = homeDir;
      })

      ./programs/autorandr
      ./programs/bash
      ./programs/chromium
      ./programs/direnv
      ./programs/git
      ./programs/gpg
      ./programs/mpv
      ./programs/neovim
      ./programs/readline
      ./programs/texlive
      ./programs/tmux
      ./programs/vscode
      ((import ./programs/vifm) { pkgs = pkgs; myTerm = myTerm; })
      ./programs/zathura

      ((import ./services/dunst) (args // { homeDir = homeDir; }))
      ./services/gpg-agent
      ./services/syncthing

      ./scripts

      ((import ./misc/applications) args)
      ./misc/keyboard
      ./misc/xsession
    ];

  home.packages = with pkgs; [
    arandr
    brightnessctl
    tdesktop
    wpa_supplicant
  ];

  programs.man.enable = false;

  xresources.extraConfig = "XTerm*faceSize: 11";
}
