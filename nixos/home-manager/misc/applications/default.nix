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
    entr
    ffmpeg
    file-rename
    gnumake
    highlight
    iftop
    inetutils # telnet
    iotop
    jq
    killall
    lm_sensors
    lsof
    maim
    neofetch
    nmap
    nomacs
    openvpn
    p7zip
    poppler_utils
    reptyr
    ripgrep
    syncthing
    sysstat # iostat
    tldr
    unzip
    usbutils # for lsusb
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
