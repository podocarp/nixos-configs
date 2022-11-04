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
    ffmpeg
    gnumake
    highlight
    hugo
    iftop
    imagemagick
    inetutils # telnet
    inkscape
    iotop
    killall
    lm_sensors
    lsof
    maim
    neofetch
    nmap
    nodePackages.firebase-tools
    nomacs
    openvpn
    p7zip
    pavucontrol
    poppler_utils
    reptyr
    syncthing
    sysstat # iostat
    tdesktop # telegram desktop
    unzip
    usbutils # for lsusb
    xclip
    xorg.xev
    xorg.xkill
    xorg.xprop
    xterm
    zip

    (python3.withPackages(p: with p; [
      pygments
      autopep8
      pip
    ]))

    ### Fonts
    source-han-sans
    source-han-mono
    (nerdfonts.override {
      fonts = [ "DroidSansMono" ];
    })
  ];

  # This must be enabled for fonts to be installed through packages.
  fonts.fontconfig.enable = lib.mkForce true;
}
