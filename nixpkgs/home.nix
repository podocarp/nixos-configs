{ config, pkgs, lib, ... }:

let
  # It is important to change this when needed. This is a global setting for
  # many other configs.
  myTerm = "xterm";
  homeDir = "/home/pengu";
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
    # ((import ./programs/xmonad/default.nix) {
    #   pkgs = pkgs; myTerm = myTerm;
    # })
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

    # ((import ./services/dunst/default.nix) {
    #   pkgs = pkgs; homeDir = homeDir;
    # })
    ./services/gpg-agent/default.nix
    # ./services/random-background/default.nix

    ./scripts/default.nix
  ];

  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
    ### Applications
    ffmpeg
    gdb
    haskellPackages.cabal-install
    haskellPackages.cabal2nix
    haskellPackages.ghc
    haskellPackages.haskell-language-server
    # haskellPackages.xmobar
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
    openvpn
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
    brightnessctl
    bind # nslookup
    iftop
    iotop
    glxinfo
    nmap
    sysstat
    telnet
    thinkfan
    xdotool # vimtex might need this
    xorg.xev
    xorg.xprop

    ### Fonts
    source-han-mono
    inconsolata
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
    windowManager.command = "exec startplasma-x11";
    initExtra = ''
      # export XMODIFIERS = "@im=fcitx"
      # export XMODIFIER = "@im=fcitx"
      # export GTK_IM_MODULE = "@im=fcitx"
      # export QT_IM_MODULE = "@im=fcitx"
    '';
    profileExtra = ''
     autorandr -c
    '';
    # pointerCursor = {
    #   package = pkgs.vanilla-dmz;
    #   name = "Vanilla-DMZ";
    #   size = 64;
    # };
    # scriptPath = ".xsession-hm";
  };

  xresources.extraConfig =
  # builtins.readFile (
  #   pkgs.fetchFromGitHub {
  #     owner = "morhetz";
  #     repo = "gruvbox-contrib";
  #     rev = "master";
  #     sha256 = "181irx5jas3iqqdlc6v34673p2s6bsr8l0nqbs8gsv88r8q066l6";
  #   } + "/xresources/gruvbox-light.xresources"
  #   ) +
  ''
  ! PaperColor Theme
  *.foreground: #4D4D4C
  *.background: #E7E8EB
  ! black
  *.color0: #EDEDED
  *.color8: #969694
  ! red
  *.color1: #D7005F
  *.color9: #D7005F
  ! green
  *.color2: #718C00
  *.color10: #718C00
  ! yellow / orange
  *.color3: #D75F00
  *.color11: #D75F00
  ! blue
  *.color4: #4271AE
  *.color12: #4271AE
  ! magenta
  *.color5: #8959A8
  *.color13: #8959A8
  ! cyan
  *.color6: #3E999F
  *.color14: #3E999F
  ! white
  *.color7: #F5F5F5
  *.color15: #2D2D2C
  XTerm*faceName: xft:Inconsolata Regular:family=mono
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
