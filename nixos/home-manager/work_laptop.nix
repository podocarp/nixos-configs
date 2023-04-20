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
      ./programs/autorandr
      ./programs/bash
      ./programs/chromium
      ./programs/direnv
      ((import ./programs/firefox) args)
      ./programs/fzf
      ./programs/git
      ./programs/gpg
      ./programs/mpv
      ./programs/navi
      ./programs/neovim
      ./programs/lazygit
      ((import ./programs/password-store) { homeDir = homeDir; })
      ./programs/readline
      ((import ./programs/rofi) { myTerm = myTerm; })
      ./programs/texlive
      ./programs/tmux
      ./programs/vscode
      ((import ./programs/vifm) (args // { myTerm = myTerm; }))
      ./programs/zathura

      ((import ./services/dunst) (args // { homeDir = homeDir; }))
      ./services/gpg-agent
      ./services/syncthing

      ./scripts

      ((import ./misc/applications) args)
      ./misc/keyboard
      ((import ./misc/xsession) (args // { myTerm = myTerm; }))
    ];

  home.packages = with pkgs; [
    arandr
    brightnessctl
    gcc
    # mysql-workbench
    tdesktop
    wpa_supplicant

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
