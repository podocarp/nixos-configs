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
    ((import ./programs/rofi/default.nix) {
      myTerm = myTerm;
    })
    ((import ./programs/password-store/default.nix) {
      homeDir = homeDir;
    })

    ./programs/bash/default.nix
    ./programs/chromium/default.nix
    ./programs/direnv/default.nix
    ./programs/git/default.nix
    ./programs/gpg/default.nix
    ./programs/neovim/default.nix
    ./programs/readline/default.nix
    ./programs/texlive/default.nix
    ./programs/tmux/default.nix
    ((import ./programs/vifm/default.nix) { pkgs = pkgs; myTerm = myTerm; })

    ./services/gpg-agent/default.nix

    ./scripts/default.nix

    ./misc/keyboard/default.nix
    ./misc/xsession/default.nix
  ];

  home.packages = with pkgs; [
    highlight
    hugo
    inetutils
    killall
    neovim-remote
    nmap
    nodePackages.firebase-tools
    unzip
    xclip
    xorg.xev
    xorg.xkill
    xorg.xprop
    xterm
    zip
  ];

  xresources.extraConfig = "XTerm*faceSize: 11";
}
