{ pkgs, lib, ... }:
{
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [
    (self: super: {
      octaveWithSym = super.octaveFull.withPackages(p: with p; [
        symbolic
      ]);
    })
  ];

  home.packages = with pkgs; [
    ### Applications
    bind # for nslookup
    dolphin
    ffmpeg
    highlight
    hugo
    iftop
    imagemagick
    inetutils # telnet
    inkscape
    iotop
    lm_sensors
    neofetch
    neovim-remote
    nmap
    nodePackages.firebase-tools
    nomacs
    okular
    openvpn
    pavucontrol
    poppler_utils
    scrot
    stress
    syncthing
    sysstat # iostat
    tdesktop # telegram desktop
    unzip
    usbutils # for lsusb
    veracrypt
    xclip
    xorg.xev
    xorg.xprop
    xterm
    zip

    (python3.withPackages(p: with p; [
      pip
      pygments
    ]))

    ### Fonts
    source-han-sans
    source-han-mono
    julia-mono
  ];

  # This must be enabled for fonts to be installed through packages.
  fonts.fontconfig.enable = lib.mkForce true;
}
