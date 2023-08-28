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
      ./programs/fzf
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

  programs.man.enable = false;

  programs.bash = {
    sessionVariables = {
      CLICOLOR = "1";
      LSCOLORS = "Exfxcxdxbxegedabagacad";
    };
    initExtra = ''
      [ -f "$HOME/.bytebm/config/config.sh" ] && . "$HOME/.bytebm/config/config.sh"
    '';
  };

  programs.go = {
    enable = true;
    goPrivate = [
      "*.byted.org"
    ];
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

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowBroken = true;
}
