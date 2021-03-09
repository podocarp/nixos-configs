{ config, pkgs, lib, ... }:

let
  # It is important to change this when needed. This is a global setting for
  # many other configs.
  myTerm = "xterm";
  homeDir = /home/pengu;
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "pengu";
  home.homeDirectory = homeDir;

  imports = [
    # this contains xmobar configs as well.
    ((import ./programs/xmonad/default.nix) {
      pkgs = pkgs; myTerm = myTerm;
    })
    ((import ./programs/rofi/default.nix) {
      myTerm = myTerm;
    })
    ((import ./programs/vifm/default.nix) {
      myTerm = myTerm;
    })
    ((import ./programs/password-store/default.nix) {
      homeDir = homeDir;
    })

    ./programs/autorandr/default.nix
    ./programs/bash/default.nix
    ./programs/chromium/default.nix
    ./programs/firefox/default.nix
    ./programs/git/default.nix
    ./programs/gpg/default.nix
    ./programs/mpv/default.nix
    ./programs/neovim/default.nix
    ./programs/readline/default.nix
    ./programs/texlive/default.nix
    ./programs/tmux/default.nix
    ./programs/vscode/default.nix
    ./programs/zathura/default.nix

    ((import ./services/dunst/default.nix) {
      pkgs = pkgs; homeDir = homeDir;
    })
    ./services/gpg-agent/default.nix
    ./services/random-background/default.nix

    ./scripts/default.nix
  ];

  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
    ### Applications
    ffmpeg
    haskellPackages.cabal-install
    haskellPackages.cabal2nix
    haskellPackages.ghc
    haskellPackages.haskell-language-server
    haskellPackages.xmobar
    highlight
    hugo
    imagemagick
    inkscape
    libnotify # for dunst
    neofetch
    neovim-remote
    nodePackages.node2nix
    nodePackages.firebase-tools
    octaveFull
    poppler_utils
    pavucontrol
    (python38.withPackages(p: with p; [
        matplotlib
        numpy
        scipy
      ]
    ))
    python38Packages.pygments
    scrot
    sxiv
    syncthing
    tdesktop # telegram desktop
    unzip
    vifm
    xterm
    yarn
    yarn2nix
    zip

    ### Admin things and other tools
    arandr
    iftop
    glxinfo
    nmap
    sysstat
    telnet
    thinkfan
    xdotool # vimtex might need this
    xorg.xbacklight
    xorg.xev
    xorg.xprop

    ### Fonts
    source-han-mono
    tamsyn
  ];

  # Note that some files are pulled in by the imports.
  home.file = {
  };

  # Used for a particular nvim snippet.
  xdg.configFile.inkscapeTemplate = {
    source = builtins.fetchurl {
      url = "https://raw.githubusercontent.com/podocarp/riceDumpling/master/.config/inkscape/templates/default1024.svg";
      sha256 = "1kyasmdv02ylk6qz3kxv46rp3czxv5ss07cs91dacadpy4l1dfrq";
    };
    target = "inkscape/templates/default1024.svg";
  };

  # This must be enabled for fonts to be installed through packages.
  fonts.fontconfig.enable = true;

  nixpkgs.config.packageOverrides = pkgs : {
    nur = import
      (builtins.fetchTarball
        "https://github.com/nix-community/NUR/archive/master.tar.gz") {
      inherit pkgs;
    };
  };

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You can update Home Manager without changing this value. See the Home
  # Manager release notes for a list of state version changes in each release.
  home.stateVersion = "21.03";

  xsession = {
    enable = true;
    initExtra = ''
      # export XMODIFIERS = "@im=fcitx"
      # export XMODIFIER = "@im=fcitx"
      # export GTK_IM_MODULE = "@im=fcitx"
      # export QT_IM_MODULE = "@im=fcitx"
    '';
    profileExtra = ''
     autorandr -c
    '';
    pointerCursor = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
      size = 32;
    };
  };

  gtk.enable = true;
  gtk.theme.package = pkgs.equilux-theme;
  gtk.theme.name = "Equilux";

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
