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

    ./programs/bash/default.nix
    ./programs/chromium/default.nix
    ./programs/firefox/default.nix
    ./programs/git/default.nix
    ./programs/neovim/default.nix
    ./programs/texlive/default.nix
    ./programs/tmux/default.nix
    ./programs/vifm/default.nix

    ./services/dunst/default.nix
    ./services/random-background/default.nix
  ];

  home.packages = with pkgs; [
    (st.overrideAttrs (oldAttrs: {
      src = fetchFromGitHub {
          owner = "podocarp";
          repo = "st";
          rev = "head";
          sha256 = "0lgg8qiimglar3dr00jn6w4w3zsr6nfmbhdf6p2q3y2nxjvl5baj";
      };
    }))


    ### Applications
    exiftool # needed for vifm sixel
    ghc
    haskellPackages.xmobar
    highlight
    hugo
    imagemagick
    inkscape
    libsixel
    neofetch
    okular
    scrot
    sxiv
    syncthing
    vifm
    xterm

    ### Admin things and other tools
    bc
    iftop
    sysstat
    xorg.xbacklight
    xorg.xev
    xorg.xprop
  ];

  home.file = {
    riceDumpling = {
      source = pkgs.fetchFromGitHub {
        owner = "podocarp";
        repo = "riceDumpling";
        rev = "master";
        # sha256 = lib.fakeSha256;
        sha256 = "00fk83rw6zcnnz6mnii0nnhcbvbagccnmf77x9li00mzmkynnayk";
      };
      target = "Documents/riceDumpling";
    };

    wallpapers = {
      source = builtins.fetchTarball "https://jiaxiaodong.com/img/wallpapers/Wallpapers.tar";
      target = "Images/wallpapers";
    };

    "Scripts" = {
      source = ./scripts;
    };
  };

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

  xsession.enable = true;
  xsession.initExtra = ''
    export XMODIFIERS = "@im=fcitx"
    export XMODIFIER = "@im=fcitx"
    export GTK_IM_MODULE = "@im=fcitx"
    export QT_IM_MODULE = "@im=fcitx"
  '';

  xresources.extraConfig = builtins.readFile (
    pkgs.fetchFromGitHub {
      owner = "dracula";
      repo = "xresources";
      rev = "master";
      sha256 = "12wmjynk0ryxgwb0hg4kvhhf886yvjzkp96a5bi9j0ryf3pc9kx7";
    } + "/Xresources"
  ) + ''
  XTerm*faceName: Dina
  XTerm*faceSize: 10
  XTerm.vt100.translations: #override \n\
    Ctrl <Key> minus: smaller-vt-font() \n\
    Ctrl <Key> plus: larger-vt-font() \n\
    Ctrl <Key> 0: set-vt-font(d) \n\
    Ctrl Shift <Key> C: copy-selection(CLIPBOARD) \n\
    Ctrl Shift <Key> V: insert-selection(CLIPBOARD) \n\
    Ctrl <Key> 0: set-vt-font(d)

  Xterm.ttyModes: erase ^?
  XTerm*decTerminalID: vt340
  XTerm*backarrowKey: false
  XTerm*bellIsUrgent: true
  XTerm*selectToClipboard: true
  XTerm*trimSelection: true
  XTerm.termName: xterm-256color
  st.font: Dina:size=11:antialias=false:autohint=false
  st.termname: st-256color
  '';
}
