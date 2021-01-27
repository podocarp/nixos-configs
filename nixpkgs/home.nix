{ config, pkgs, lib, ... }:

let
  homeDir = /home/pengu;
  configDir = "/home/pengu/Documents/riceDumpling";
  myTerm = "xterm";
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "pengu";
  home.homeDirectory = homeDir;

  imports = [
    # xmonad configuration contains xmobar as well.
    ((import ./programs/xmonad/default.nix) {
      pkgs = pkgs; myTerm = myTerm;
    })
    ((import ./programs/rofi/default.nix) {
      myTerm = myTerm;
    })
    ((import ./programs/vifm/default.nix) {
        myTerm = myTerm;
    })

    ./programs/autorandr/default.nix
    ./programs/bash/default.nix
    ./programs/chromium/default.nix
    ./programs/firefox/default.nix
    ./programs/git/default.nix
    ./programs/neovim/default.nix
    ./programs/readline/default.nix
    ./programs/texlive/default.nix
    ./programs/tmux/default.nix

    ./services/dunst/default.nix
    ./services/random-background/default.nix
  ];

  home.packages = with pkgs; [
    ### Applications
    haskellPackages.cabal-install
    haskellPackages.cabal2nix
    haskellPackages.ghc
    haskellPackages.haskell-language-server
    haskellPackages.xmobar
    highlight
    hugo
    imagemagick
    inkscape
    neofetch
    okular
    poppler_utils
    pavucontrol
    python38Packages.pygments
    scrot
    sxiv
    syncthing
    vifm
    xterm

    ### Admin things and other tools
    arandr
    iftop
    sysstat
    thinkfan
    xorg.xbacklight
    xorg.xev
    xorg.xprop

    ### Fonts
    source-han-mono
    tamsyn
  ];

  home.file = {
    wallpapers = {
      source = builtins.fetchTarball {
        url = "https://jiaxiaodong.com/img/wallpapers/Wallpapers.tar";
        sha256 = "10992gd3z77r3jaz5dnk0w3ql1nys9sz6swr1n8irxrxw8iqqr9g";
      };
      target = "Images/wallpapers";
    };

    "Scripts" = {
      source = ./scripts;
    };
  };

  fonts.fontconfig.enable = true;

  nixpkgs.config.packageOverrides = pkgs : {
    nur = import
      (builtins.fetchTarball
        "https://github.com/nix-community/NUR/archive/master.tar.gz") {
      inherit pkgs;
    };
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";

  xsession = {
    enable = true;
    initExtra = ''
      export XMODIFIERS = "@im=fcitx"
      export XMODIFIER = "@im=fcitx"
      export GTK_IM_MODULE = "@im=fcitx"
      export QT_IM_MODULE = "@im=fcitx"
      autorandr -c
    '';
    profileExtra = ''
      autorandr -c
    '';
  };

  xresources.extraConfig = builtins.readFile (
    pkgs.fetchFromGitHub {
      owner = "dracula";
      repo = "xresources";
      rev = "master";
      sha256 = "12wmjynk0ryxgwb0hg4kvhhf886yvjzkp96a5bi9j0ryf3pc9kx7";
    } + "/Xresources"
  ) + ''
  XTerm*faceName: Tamsyn
  XTerm*faceSize: 10
  XTerm.vt100.translations: #override \n\
    Ctrl <Key> minus: smaller-vt-font() \n\
    Ctrl <Key> plus: larger-vt-font() \n\
    Ctrl <Key> 0: set-vt-font(d) \n\
    Ctrl Shift <Key> C: copy-selection(CLIPBOARD) \n\
    Ctrl Shift <Key> V: insert-selection(CLIPBOARD) \n\
    Ctrl <Key> 0: set-vt-font(d)
  XTerm*backarrowKey: false
  XTerm.ttyModes: erase ^?
  XTerm*ptyInitialErase: true
  XTerm*decTerminalID: vt340
  XTerm*numColorRegisters: 256
  XTerm*bellIsUrgent: true
  XTerm*selectToClipboard: true
  XTerm*trimSelection: true
  XTerm.termName: st-256color
  '';
}
