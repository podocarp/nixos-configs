{ pkgs, lib, ... }:
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

  imports = [
    ./programs/bash
    ./programs/direnv
    ./programs/atuin
    ./programs/git
    ./programs/neovim
    ./programs/nixvim
    ./programs/readline
    ./programs/tmux
    ./programs/vscode
    ((import ./programs/vifm) {
      pkgs = pkgs;
      myTerm = myTerm;
    })

    ./services/syncthing

    ./scripts

    ./misc/keyboard
  ];

  home.packages = with pkgs; [
    bind # for nslookup
    entr # run arbitrary commands when files change
    ffmpeg
    file-rename
    gnumake
    highlight
    iftop
    inetutils # telnet
    jq
    keepassxc
    lsof
    nix-index
    nmap
    p7zip
    (python3.withPackages (
      p: with p; [
        requests
      ]
    ))
    ripgrep
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
    shellAliases = {
      bs = "byteshell --idc i18n --psm";
    };
    initExtra = ''
      [ -f "$HOME/.bytebm/config/config.sh" ] && . "$HOME/.bytebm/config/config.sh"
      eval "$(/opt/homebrew/bin/brew shellenv)"
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
      Include "/Users/bytedance/.byted-ide/ssh_config"
      Include "/Users/bytedance/Library/Application Support/cloudide-cli/*/ssh.d/*"

      Host cloudide-portal
        HostName ws5fa4b31587b88416
        User byteide
        ProxyCommand /Users/bytedance/.local/bin/cloudide-cli --log-level info --apiserver-baseurl https://ide-us.tiktok-row.org --tenant-name bytedance workspace ssh proxy --id %h
        StrictHostKeyChecking no

      Host cloudide-robin
        HostName ws88adb68d478e9a82
        User byteide
        ProxyCommand /Users/bytedance/.local/bin/cloudide-cli --log-level info --apiserver-baseurl https://ide-us.tiktok-row.org --tenant-name bytedance workspace ssh proxy --id %h
        StrictHostKeyChecking no

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

      Host jump-proxy-sg
          HostName jump-proxy-sg.byted.org
      Host jump-proxy-us
          HostName jump-us.itbyted.org
    '';
  };

  programs.vscode.userSettings = {
    "extensions.supportUntrustedWorkspaces" = {
      "byted-ide.dev-environment-manager" = {
        "supported" = true;
      };
    };
    "remote.SSH.defaultExtensions" = [ "byted-ide.gallery" ];
  };

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowBroken = true;
}
