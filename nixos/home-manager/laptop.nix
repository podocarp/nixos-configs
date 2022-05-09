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

  imports =
  [
    ((import ./programs/xmonad/default.nix) {
      pkgs = pkgs; myTerm = myTerm;
    })
    ((import ./programs/rofi/default.nix) {
      myTerm = myTerm;
    })
    ((import ./programs/password-store/default.nix) {
      homeDir = homeDir;
    })

    ./programs/autorandr/default.nix
    ./programs/bash/default.nix
    ./programs/chromium/default.nix
    ((import ./programs/firefox/default.nix) args)
    ./programs/git/default.nix
    ./programs/gpg/default.nix
    ./programs/java/default.nix
    ./programs/mpv/default.nix
    ./programs/neovim/default.nix
    ./programs/readline/default.nix
    ./programs/texlive/default.nix
    ./programs/tmux/default.nix
    # ./programs/vscode/default.nix
    ((import ./programs/vifm/default.nix) { pkgs = pkgs; myTerm = myTerm; })
    ./programs/zathura/default.nix

    ((import ./services/dunst/default.nix) (args // { homeDir = homeDir; }))
    ./services/gpg-agent/default.nix
    ./services/syncthing/default.nix
    ./services/random-background/default.nix

    ./scripts/default.nix

    ((import ./misc/fcitx/default.nix) args)
    ((import ./misc/applications/default.nix) args)
    ./misc/keyboard/default.nix
    ./misc/xsession/default.nix
  ];

  nixpkgs.config.packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
      inherit pkgs;
    };
  };

  home.packages = with pkgs; [
    arandr
    brightnessctl
    wpa_supplicant
  ];

  xresources.extraConfig = "XTerm*faceSize: 11";
}
