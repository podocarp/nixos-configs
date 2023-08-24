args@{ config, pkgs, lib, ... }:
let
  myTerm = "iterm";
  homeDir = "/Users/bytedance";
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  home.username = "bytedance";
  home.homeDirectory = homeDir;
  home.stateVersion = "23.11";

  imports =
    [
      ((import ./programs/password-store) {
        homeDir = homeDir;
      })

      ./programs/bash
      ./programs/direnv
      ./programs/git
      ./programs/mpv
      ./programs/neovim
      ./programs/readline
      ./programs/tmux
      ./programs/vscode
      ((import ./programs/vifm) { pkgs = pkgs; myTerm = myTerm; })

      ./services/syncthing

      ./scripts

      ./misc/keyboard
    ];

  home.packages = with pkgs; [
    bind # for nslookup
    entr
    ffmpeg
    file-rename
    gnumake
    highlight
    iftop
    inetutils # telnet
    jq
    lsof
    nix-index
    nmap
    nomacs
    p7zip
    ripgrep
    syncthing
    tldr
    unzip
    unrar
    zip
  ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowBroken = true;
  programs.man.enable = false;
}
