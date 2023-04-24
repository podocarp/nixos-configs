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
  home.username = "pengu";
  home.homeDirectory = homeDir;
  home.stateVersion = "22.11";

  imports = [
    ./programs/bash
    ./programs/direnv
    ./programs/fzf
    ./programs/git
    ./programs/gpg
    ./programs/neovim
    ((import ./programs/password-store) {
      homeDir = homeDir;
    })
    ./programs/readline
    ./programs/tmux
    ((import ./programs/vifm) {
      pkgs = pkgs;
      myTerm = myTerm;
    })

    ./services/gpg-agent

    ./misc/applications
    ./scripts
  ];

  home.packages = with pkgs; [
    (python3.withPackages (p: with p; [
      requests
    ]))
    stress
    sysstat
    yt-dlp
  ];
}

