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
    # ./programs/firefox/default.nix
    ./programs/git/default.nix
    ./programs/gpg/default.nix
    ./programs/java/default.nix
    ./programs/mpv/default.nix
    ./programs/neovim/default.nix
    ./programs/readline/default.nix
    ./programs/texlive/default.nix
    ./programs/tmux/default.nix
    ./programs/vscode/default.nix
    ./programs/zathura/default.nix

    # ((import ./services/dunst/default.nix) {
    #   pkgs = pkgs; config = config; homeDir = homeDir;
    # })
    ./services/gpg-agent/default.nix
    # ./services/random-background/default.nix

    ./scripts/default.nix
  ];

  nixpkgs.overlays = [
    # (self: super: {
    #   gajim = super.gajim.override {
    #     extraPythonPackages = ps: with ps; [ pygments ];
    #   };
    # })
  ];

  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
    ### Applications
    bind # for nslookup
    ffmpeg
    gcc
    gdb
    glxinfo
    gnumake
    haskellPackages.cabal-install
    haskellPackages.cabal2nix
    haskellPackages.ghc
    haskellPackages.haskell-language-server
    highlight
    hugo
    iftop
    imagemagick
    inkscape
    iotop
    jmtpfs
    kfind
    lm_sensors
    neofetch
    neovim-remote
    nmap
    nodePackages.firebase-tools
    nomacs
    octaveFull
    okular
    openvpn
    poppler_utils
    scrot
    stress
    syncthing
    sysstat
    tdesktop # telegram desktop
    telnet
    unzip
    usbutils # for lsusb
    veracrypt
    vifm
    xdotool # vimtex needs this
    xorg.xev
    xorg.xprop
    xterm
    yarn
    yt-dlp
    zip

    (python3.withPackages(p: with p; [
        jupyterlab
        matplotlib
        numpy
        pandas
        pip
        scipy
      ]
    ))

    ### Fonts
    source-han-mono
    julia-mono
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
    # nur = import
    #   (builtins.fetchTarball
    #     "https://github.com/nix-community/NUR/archive/master.tar.gz") {
    #   inherit pkgs;
    # };
  };

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
      *.background: #FFFFFF
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
      XTerm*faceName: xft:JuliaMono
      XTerm*faceSize: 10
      xterm*renderFont: true
      xterm*faceSize1: 10
      xterm*faceSize2: 12
      xterm*faceSize3: 15
      xterm*faceSize4: 18
      xterm*faceSize5: 21
      xterm*faceSize6: 25
      xterm*forceBoxChars: false
      xterm*scaleHeight: 1.0
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
