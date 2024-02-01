args@{ config, pkgs, lib, ... }:
let
  myTerm = "iterm";
  homeDir = "/Users/jxd";
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  home.username = "jxd";
  home.homeDirectory = homeDir;
  home.stateVersion = "23.11";

  imports =
    [
      ./programs/bash
      ./programs/direnv
      ./programs/fzf
      ./programs/git
      ./programs/neovim
      ./programs/readline
      ./programs/tmux

      ./services/syncthing

      ./scripts

      # ./misc/keyboard
    ];

  home.packages = with pkgs; [
    bind # for nslookup
    ffmpeg
    file-rename
    gnumake
    highlight
    jq
    keepassxc
    lsof
    nix-index
    p7zip
    (python3.withPackages (p: with p; [
      requests
      flake8
      autopep8
    ]))
    ripgrep
    syncthing
    tldr
    unzip
    unrar
    zip
  ];

  programs.bash = {
    sessionVariables = {
      CLICOLOR = "1";
      LSCOLORS = "Exfxcxdxbxegedabagacad";
    };
    shellAliases = { };
    initExtra = ''
    '';
  };

  programs.go = {
    enable = true;
  };

  programs.ssh = {
    enable = true;
    extraConfig = ''
    '';
  };

  nixpkgs.config.allowUnfree = true;
}
