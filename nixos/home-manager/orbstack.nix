args@{ config, pkgs, lib, ... }:
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  home.stateVersion = "22.11";

  imports =
    [
      ./programs/bash
      ./programs/direnv
      ./programs/fzf
      ./programs/git
      ./programs/gpg
      ./programs/navi
      ./programs/neovim
      ./programs/lazygit
      ./programs/readline
      ./programs/tmux
      ./programs/vscode
      ((import ./programs/vifm) (args // { myTerm = "xterm"; }))

      ./services/gpg-agent

      ./scripts

      ((import ./misc/applications) args)
    ];

  home.packages = with pkgs; [
    gcc
    wpa_supplicant

    (python3.withPackages (p: with p; [
      autopep8
      flake8
      mypy
      requests
    ]))
  ];

  programs.bash = {
    sessionVariables = { };
  };

  programs.go = {
    enable = true;
    package = pkgs.go_1_18;
  };

  programs.git = {
    userName = lib.mkForce "Jia Xiaodong";
    userEmail = lib.mkForce "xiaodong.jia@bytedance.com";
    extraConfig = {
      url = { };
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
}
