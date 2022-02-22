{ pkgs, lib, ... }:
{
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

  # This must be enabled for fonts to be installed through packages.
  fonts.fontconfig.enable = lib.mkForce true;
}
