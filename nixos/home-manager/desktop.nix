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
    ((import ./programs/password-store/default.nix) {
      homeDir = homeDir;
    })

    ./programs/autorandr/default.nix
    ./programs/bash/default.nix
    ./programs/chromium/default.nix
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

    ./services/gpg-agent/default.nix
    ./services/syncthing/default.nix

    ./scripts/default.nix
  ];

  nixpkgs.overlays = [
    (self: super: {
    # gajim = super.gajim.override {
    #   extraPythonPackages = ps: with ps; [ pygments ];
    # };
    })
  ];

  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
    ### Applications
    bind # for nslookup
    ffmpeg
    gcc
    gdb
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
    kfind
    lm_sensors
    neofetch
    neovim-remote
    nmap
    nodePackages.firebase-tools
    nomacs
    okular
    openvpn
    poppler_utils
    ranger
    scrot
    stress
    syncthing
    sysstat
    tdesktop # telegram desktop
    telnet
    unzip
    usbutils # for lsusb
    veracrypt
    xdotool
    xclip
    xorg.xev
    xorg.xprop
    xterm
    yarn
    yt-dlp
    zip

    (octaveFull.withPackages(p: with p; [
      symbolic
    ]))

    (python3.withPackages(p: with p; [
      matplotlib
      numpy
      pygments
      scipy
    ]))

    ### Fonts
    source-han-sans
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
  fonts.fontconfig.enable = lib.mkForce true;

  i18n.inputMethod.enabled = "fcitx";
  i18n.inputMethod.fcitx.engines = with pkgs.fcitx-engines; [ libpinyin ];

  nixpkgs.config.packageOverrides = pkgs : {
  };

  # xdg.configFile."plasma-workspace/env/set_window_manager.sh" = {
  #     text = "export KDEWM=${pkgs.xmonad-with-packages}/bin/xmonad";
  # };
  xsession = {
    enable = true;
    windowManager.command = lib.mkForce "exec startplasma-x11";
    initExtra = ''
      export XMODIFIERS = "@im=fcitx"
      export XMODIFIER = "@im=fcitx"
      export GTK_IM_MODULE = "@im=fcitx"
      export QT_IM_MODULE = "@im=fcitx"
    '';
    profileExtra = ''
     autorandr -c
    '';
    scriptPath = ".xsession-hm";
  };

  xresources.extraConfig =
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
    XTerm*forceBoxChars: false
    XTerm.vt100.translations: #override \n\
      Ctrl <Key> minus: smaller-vt-font() \n\
      Ctrl <Key> plus: larger-vt-font() \n\
      Ctrl <Key> 0: set-vt-font(d) \n\
      Ctrl Shift <Key> C: copy-selection(CLIPBOARD) \n\
      Ctrl Shift <Key> V: insert-selection(CLIPBOARD)
    XTerm*backarrowKey: false
    XTerm*ttyModes: erase ^?
    XTerm*ptyInitialErase: true
    XTerm*decTerminalID: vt340
    XTerm*trimSelection: true
    XTerm*termName: st-256color
    XTerm*pointerShape: left_ptr
    XTerm*pointerColor: black
    XTerm*pointerColorBackground: black
  '';
}
