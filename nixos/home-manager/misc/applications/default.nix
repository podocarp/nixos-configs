{ pkgs, lib, ... }:
{
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [
    (self: super: {
      octaveWithSym = super.octaveFull.withPackages (p: with p; [
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
