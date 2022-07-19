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
    ./programs/gpg/default.nix
    ./programs/java/default.nix
    ./programs/mpv/default.nix
    ./programs/neovim/default.nix
    ./programs/readline/default.nix
    ./programs/texlive/default.nix
    ./programs/tmux/default.nix
    ./programs/vscode/default.nix
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

  home.packages =
    let
      mypkgs = with pkgs; {
        smc = callPackage ../derivations/smc.drv args;
      };
    in
    with pkgs; [
    arandr
    brightnessctl
    wpa_supplicant
    jetbrains.goland
    jetbrains.pycharm-professional
    zoom-us
    gcc
  ];

  programs.bash = {
    sessionVariables = {
      GOPROXY = "https://nexus.shopee.io/,direct";
      GOPATH = "$HOME/go";
      GOPRIVATE = "*.garena.com";
    };
  };

  programs.go = {
    enable = true;
    goPrivate = [ "*.garena.com" ];
  };

  programs.git = {
    enable = true;
    extraConfig = {
      user = {
        name = "Jia Xiaodong";
        email = "xiaodong.jia@shopee.com";
      };
      url = {
        "ssh://gitlab@git.garena.com:2222" = { insteadOf = "https://git.garena.com"; };
      };
    };
  };

  programs.ssh = {
    enable = true;
    extraConfig = ''
      Host *
        HostKeyAlgorithms +ssh-rsa
        PubkeyAcceptedKeyTypes +ssh-rsa
        PubkeyAcceptedAlgorithms +ssh-rsa
    '';
  };

  xresources.extraConfig = "XTerm*faceSize: 11";
}
