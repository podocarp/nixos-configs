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
      ./programs/autorandr/default.nix
      ./programs/bash/default.nix
      ./programs/chromium/default.nix
      ./programs/direnv/default.nix
      ((import ./programs/firefox/default.nix) args)
      ./programs/git/default.nix
      ./programs/gpg/default.nix
      ./programs/mpv/default.nix
      ./programs/neovim/default.nix
      ((import ./programs/password-store/default.nix) { homeDir = homeDir; })
      ./programs/readline/default.nix
      ((import ./programs/rofi/default.nix) { myTerm = myTerm; })
      ./programs/texlive/default.nix
      ./programs/tmux/default.nix
      ./programs/vscode/default.nix
      ((import ./programs/vifm/default.nix) (args // { myTerm = myTerm; }))
      ./programs/zathura/default.nix

      ((import ./services/dunst/default.nix) (args // { homeDir = homeDir; }))
      ./services/gpg-agent/default.nix
      ./services/syncthing/default.nix

      ./scripts/default.nix

      ((import ./misc/applications/default.nix) args)
      ./misc/keyboard/default.nix
      ((import ./misc/xsession/default.nix) (args // { myTerm = myTerm; }))
    ];

  home.packages = with pkgs; [
    arandr
    brightnessctl
    gcc
    mysql-workbench
    tdesktop
    wpa_supplicant
    zoom-us

    (python3.withPackages (p: with p; [
      autopep8
      flake8
      mypy
      requests
    ]))
  ];

  programs.bash = {
    sessionVariables = {
      GOPROXY = "https://nexus.shopee.io/,direct";
      GOPATH = "$HOME/go";
      GOPRIVATE = "*.garena.com";
      ENV = "test";
      LOG_LEVEL = "debug";
    };
  };

  programs.go = {
    enable = true;
    package = pkgs.go_1_18;
    goPrivate = [ "*.garena.com" ];
  };

  programs.git = {
    userName = lib.mkForce "Jia Xiaodong";
    userEmail = lib.mkForce "xiaodong.jia@shopee.com";
    extraConfig = {
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
        ForwardAgent yes
    '';
  };

  xresources.extraConfig = "XTerm*faceSize: 11";
}
