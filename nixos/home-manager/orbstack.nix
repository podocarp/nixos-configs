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
    sessionVariables = {
      GOPRIVATE = "*.byted.org,*.everphoto.cn,git.smartisan.com";
    };
  };

  programs.go = {
    enable = true;
    package = pkgs.go_1_18;
  };

  programs.git = {
    userName = lib.mkForce "xiaodong.jia";
    userEmail = lib.mkForce "xiaodong.jia@bytedance.com";
    extraConfig = {
      url = {
        "ssh://xiaodong.jia@git.byted.org:29418" = {
          insteadOf = [
            "https://git.byted.org"
            "https://review.byted.org"
          ];
        };
        "git@code.byted.org:" = {
          insteadOf = "https://code.byted.org";
        };
      };
    };
  };

  programs.ssh = {
    enable = true;
    extraConfig = ''
      Host *
          GSSAPIAuthentication yes
          GSSAPIDelegateCredentials no
          HostKeyAlgorithms +ssh-rsa
          PubkeyAcceptedKeyTypes +ssh-rsa
          PubkeyAcceptedAlgorithms +ssh-rsa
      Host git.byted.org
          Hostname git.byted.org
          Port 29418
          User xiaodong.jia
      Host review.byted.org
          Hostname git.byted.org
          Port 29418
          User xiaodong.jia
      Host *.byted.org
          GSSAPIAuthentication yes
          User xiaodong.jia
      Host devbox
          HostName 10.37.83.176
          User xiaodong.jia
          GSSAPIAuthentication yes
    '';
  };
}
